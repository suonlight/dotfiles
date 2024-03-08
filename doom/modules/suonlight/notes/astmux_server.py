import re
import subprocess
import uuid
import time
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

@server.register_function
def execute_astmux(session, lang, command):
    try:
        # Generate a unique UUID
        unique_id = str(uuid.uuid4())

        # Modify the command to include the UUID
        if lang == 'ruby':
            wrapped_command = f'puts "# start:{unique_id}"; {command}; puts "# finish:{unique_id}"'
        elif lang == 'sh':
            wrapped_command = f'echo "# start:{unique_id}"; {command}; echo "# finish:{unique_id}"'
        else:
            return {'status': 'error', 'message': 'Unsupported language'}

        # Run the modified command using tmux
        subprocess.run(['tmux', 'send-keys', '-t', session, wrapped_command, 'C-m'], check=True)

        # Periodically get tmux pane output using tmux capture-pane
        output = ""
        while True:
            time.sleep(1)  # Adjust the sleep interval as needed
            capture_process = subprocess.run([
                'tmux', 'capture-pane', '-J', '-p', '-S', '-', '-t', session
            ], capture_output=True, text=True, check=False)
            captured_output = capture_process.stdout

            output += captured_output
            finish_match = re.search(f"# finish:{unique_id}", output)
            if finish_match and finish_match.start() != finish_match.end():
                break

        original_output = output
        start_match = re.search(f"# start:{unique_id}", output)
        finish_match = re.search(f"# finish:{unique_id}", output)

        if start_match and finish_match:
            start_index = start_match.end()
            output = output[start_index:]

            print("---------------------")
            print(output)

            output = re.sub(f"# start:{uuid}", "", output)
            output = re.sub(f"# finish:{uuid}", "", output)

            return {'start': start_match.end(), 'output': output, 'aaa': original_output }
        else:
            return {'start': start_match, 'finish': finish_match, 'output': output }

    except subprocess.CalledProcessError as e:
        return {'status': 'error', 'message': str(e)}

server.print_port()
server.serve_forever()
