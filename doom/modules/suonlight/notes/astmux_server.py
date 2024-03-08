import re
import subprocess
import uuid
import time
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

def wrap_command(jid, lang, command):
    command = '; '.join(map(lambda x: x.strip(), command.split("\n")))
    if lang == 'ruby':
        return f'puts "# start:{jid}"; {command}; puts "# finish:{jid}"'
    elif lang == 'sh':
        return f'echo "# start:{jid}"; {command}; echo "# finish:{jid}"'
    else:
        return None

def run_command_in_tmux(session, window, wrapped_command):
    subprocess.run(['tmux', 'send-keys', '-t', f'{session}:{window}', wrapped_command, 'C-m'], check=True)

def capture_tmux_output(session, jid):
    output = ""
    while True:
        time.sleep(1)
        capture_process = subprocess.run([
            'tmux', 'capture-pane', '-J', '-p', '-S', '-', '-t', session
        ], capture_output=True, text=True, check=False)
        captured_output = capture_process.stdout
        output += captured_output
        finish_match = re.search(f"# finish:{jid}", output)
        if finish_match and finish_match.start() != finish_match.end():
            break

    return extract_output(jid, output)

def extract_output(jid, output):
    start_match = re.search(f"\n# start:{jid}", output)
    finish_match = re.search(f"\n# finish:{jid}", output)

    if start_match and finish_match:
        start_index = start_match.end()
        finish_index = finish_match.start()
        extracted_output = output[start_index:finish_index]

        extracted_output = re.sub(f"# start:{uuid}", "", extracted_output)
        extracted_output = re.sub(f"# finish:{uuid}", "", extracted_output)
        extracted_output = extracted_output.strip()

        return extracted_output
    else:
        return None

def check_session_existence(session):
    try:
        subprocess.run(['tmux', 'has-session', '-t', session], check=True)
        return True
    except subprocess.CalledProcessError:
        return False

def check_window_existence(session, window):
    try:
        subprocess.run(['tmux', 'list-windows', '-t', f'{session}:{window}'], check=True)
        return True
    except subprocess.CalledProcessError:
        return False

def create_session(session):
    subprocess.run(['tmux', 'new-session', '-d', '-s', session], check=True)

def create_window(session, window):
  subprocess.run([
    'tmux', 'new-window',
     "-c", "~",
     '-n', window,
     '-t', session
  ], check=True)

  # sleep to wait for window to be created
  time.sleep(1)

@server.register_function
def execute_astmux(session, lang, jid, command):
    try:
        jid = str(jid)
        wrapped_command = wrap_command(jid, lang, command)

        if wrapped_command is None:
            return {'jid': jid, 'status': 'error', 'message': 'Unsupported language'}

        # Extract window from session (session_name:window_name or session_name)
        session_parts = session.split(':')
        if len(session_parts) > 1:
            session_name, window_name = session_parts[0], session_parts[1]
        else:
            session_name, window_name = session_parts[0], '0'

        if not check_session_existence(session_name):
            create_session(session_name)

        if not check_window_existence(session_name, window_name):
            create_window(session_name, window_name)

        run_command_in_tmux(session_name, window_name, wrapped_command)
        output = capture_tmux_output(session_name, jid)

        if output is not None:
            return {'jid': jid, 'status': 'ok', 'message': output}
        else:
            return {'jid': jid, 'status': 'error', 'message': output}

    except subprocess.CalledProcessError as e:
        return {'jid': jid, 'status': 'error', 'message': str(e)}

server.print_port()
server.serve_forever()
