#!/usr/bin/env python3
import os
import subprocess

import requests
import time

CLEARED_CONTENTS = """
"""
POLL_PERIOD = 4

def main():
    my_env = os.environ.copy()
    my_env["EXPERIMENTAL"] = "1"
    process = subprocess.Popen(["lamdera", "live"], env=my_env)

    try:
        while True:
            path = "src/Host.elm"
            try:
                with open(path, "r") as file_handle:
                    contents = file_handle.read()
                if contents == CLEARED_CONTENTS:
                    time.sleep(POLL_PERIOD)
                    continue
            except FileNotFoundError:
                pass

            r = requests.get("http://localhost:8000/", timeout=5)
            r.raise_for_status()
            if '"type": "compile-errors",' in r.text:
                with open(path, "w") as file_handle:
                    file_handle.write(CLEARED_CONTENTS)

                print("Problem!")
            time.sleep(POLL_PERIOD)
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
