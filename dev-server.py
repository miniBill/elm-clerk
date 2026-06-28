#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#   "requests",
# ]
# ///
import os
import subprocess

import requests
import time

CLEARED_CONTENTS = """
"""
POLL_PERIOD = 4

def main():
    print("Starting lamdera")
    my_env = os.environ.copy()
    my_env["EXPERIMENTAL"] = "1"
    process = subprocess.Popen(["lamdera", "live"], env=my_env)
    print("Started lamdera")

    try:
        while True:
            time.sleep(POLL_PERIOD)
            path = "src/Host.elm"
            try:
                with open(path, "r") as file_handle:
                    contents = file_handle.read()
                if contents == CLEARED_CONTENTS:
                    continue
            except FileNotFoundError:
                pass

            r = requests.get("http://localhost:8000/", timeout=5)
            r.raise_for_status()
            if '"type": "compile-errors",' in r.text:
                with open(path, "w") as file_handle:
                    file_handle.write(CLEARED_CONTENTS)

                print("Problem!")
    except KeyboardInterrupt:
        pass
    finally:
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                process.kill()


if __name__ == '__main__':
    main()
