import re
import subprocess
import uuid
import time
import os
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

class RubyInterativeHandler:
    @staticmethod
    def wrap_command(jid, command):
        command = '; '.join(map(lambda x: x.strip(), command.split("\n")))
        return f'puts("# start:{jid}"); 1\n {command}\n puts "# finish:{jid}"; 1\n'

    @staticmethod
    def format_output(output):
      print('format------------------')
      print(output)

      # remove the first character
      output = re.sub(r'^1', '', output)

      # replace finish marker
      output = re.sub(r'puts "# finish:[a-z0-9]+"; 1', '', output)
      # replace line started with string 'nil'
      output = re.sub(r'nil', '', output)
      # replace line started with string '=>'
      output = re.sub(r'=>', '', output)
      # replace line started with string 'pry'
      output = re.sub(r'pry', '', output)
      # replace line started with string 'irb'
      output = re.sub(r'irb', '', output)
      # replace whole line started with number [number]
      output = re.sub(r'\[\d+\].*', '', output)
      # replace empty lines
      output = re.sub(r'\n+', '\n', output)

      output = output.strip()

      return output

class ShellInterativeHandler:
    @staticmethod
    def wrap_command(jid, command):
        return f'echo "# start:{jid}"; {command}; echo "# finish:{jid}"'

    @staticmethod
    def format_output(output):
        return output.strip()

def get_command_wrapper(lang):
    if lang == 'ruby':
        return RubyInterativeHandler
    elif lang == 'sh':
        return ShellInterativeHandler
    else:
        return None

def wrap_command(jid, lang, command):
    command_wrapper = get_command_wrapper(lang)
    if command_wrapper:
        return command_wrapper.wrap_command(jid, command)
    else:
        return None

def format_output(lang, output):
    command_wrapper = get_command_wrapper(lang)
    if command_wrapper:
        return command_wrapper.format_output(output)
    else:
        return output

def run_command_in_tmux(socket, session, window, wrapped_command):
    subprocess.run(['tmux', '-S', socket, 'send-keys', '-t', f'{session}:{window}', wrapped_command, 'C-m'], check=True)

def generate_log_filename(jid):
    # You can customize the log file directory and format as needed
    return f'/tmp/astmux_{jid}.log'

def capture_tmux_output(socket, session, window, lang, jid):
    output = ""
    os.environ["PYTHONUNBUFFERED"] = "1"
    while True:
        log_filename = generate_log_filename(jid)

        cmd = ['tmux', '-S', socket, 'capture-pane', '-J', '-p', '-S', '-', '-t', f'{session}:{window}']
        subprocess.run(cmd, stdout=open(log_filename, 'w'), check=True)

        time.sleep(1)

        # open log file
        captured_output = ''
        with open(log_filename, 'r') as f:
            captured_output = f.read()

        # remove log file
        os.remove(log_filename)

        output += captured_output

        finish_match = re.search(f"# finish:{jid}", output)
        if finish_match and finish_match.start() != finish_match.end():
            break

    output = extract_output(jid, output)

    if output is not None:
        return format_output(lang, output)

    return output

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

def check_session_existence(socket, session):
    try:
        subprocess.run(['tmux', '-S', socket, 'has-session', '-t', session], check=True)
        return True
    except subprocess.CalledProcessError:
        return False

def check_window_existence(socket, session, window):
    try:
        subprocess.run(['tmux', '-S', socket, 'list-windows', '-t', f'{session}:{window}'], check=True)
        return True
    except subprocess.CalledProcessError:
        return False

def create_session(socket, session):
    subprocess.run(['tmux', '-S', socket, 'new-session', '-d', '-s', session], check=True)

def create_window(socket, session, window):
    subprocess.run([
      'tmux', '-S', socket,
      'new-window',
      '-n', window,
      '-t', session
    ], check=True)

    # sleep to wait for window creation
    time.sleep(1)

@server.register_function
def execute_astmux(socket, session, lang, jid, command):
    try:
        jid = str(jid)

        print('0000000000000000')
        print(lang)
        if lang == []:
          lang = 'sh'

        wrapped_command = wrap_command(jid, lang, command)

        if wrapped_command is None:
            return {'jid': jid, 'status': 'error', 'message': 'Unsupported language'}

        # Extract window from session (session_name:window_name or session_name)
        session_parts = session.split(':')
        if len(session_parts) > 1:
            session_name, window_name = session_parts[0], session_parts[1]
        else:
            session_name, window_name = session_parts[0], '0'

        if not check_session_existence(socket, session_name):
            create_session(socket, session_name)

        if not check_window_existence(socket, session_name, window_name):
            create_window(socket, session_name, window_name)

        run_command_in_tmux(socket, session_name, window_name, wrapped_command)
        output = capture_tmux_output(socket, session_name, window_name, lang, jid)
        print('final----------------')
        print(output)

        if output is not None:
            return {'jid': jid, 'status': 'ok', 'message': output}
        else:
            return {'jid': jid, 'status': 'error', 'message': output}

    except subprocess.CalledProcessError as e:
        return {'jid': jid, 'status': 'error', 'message': str(e)}

server.print_port()
server.serve_forever()
