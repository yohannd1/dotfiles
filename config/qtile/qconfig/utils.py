from typing import Iterable, Optional, Tuple, List, Sized
import sys
import subprocess as sp

class BadXResourceError(Exception): ...

def xgetres(resource_name: str, fallback: str = None) -> str:
    command = sp.Popen(["xgetres", resource_name],
                       stdout=sp.PIPE,
                       encoding="UTF-8")

    exit_status = command.wait()

    if exit_status == 0:
        return command.stdout.read().strip()

    if fallback is not None:
        return fallback

    raise BadXResourceError(resource_name)

def fzagnostic(
    choices: Iterable[str],
    height: Optional[int] = None,
    prompt: Optional[str] = None,
    max_choice_number: Optional[int] = None,
    starting_number: int = 0,
) -> Optional[Tuple[int, str]]:
    # set the default max number width
    if max_choice_number is None:
        max_choice_number = (
            (choices.__len__() - 1) if isinstance(choices, Sized) else 1000
        )

    # minimum length: 2 chars wide
    max_choice_number += starting_number
    max_choice_number = max(10, max_choice_number)

    prompt_list: List[str] = ["-p", prompt] if prompt is not None else []
    height_list: List[str] = ["-h", str(height)] if height is not None else []

    with sp.Popen(
        ["fzagnostic"] + prompt_list + height_list,
        stdin=sp.PIPE,
        stdout=sp.PIPE,
        encoding="UTF-8",
    ) as proc:
        if proc is None:
            raise Exception("Failed to start process")

        choice_number_size = str(max_choice_number).__len__()
        number_format = f"{{:0{choice_number_size}d}}"

        # feed choices to the stdin
        for i, line in enumerate(choices):
            print(
                f"#{number_format.format(i + starting_number)} {line}",
                file=proc.stdin,
            )

        # close the stdin
        proc.stdin.close()

        # wait for the process to finish and act according to the exit code
        if proc.wait() == 0:
            splits = proc.stdout.readline().strip().split(" ", maxsplit=1)

            if len(splits) != 2:
                print(f"Invalid input: {splits}", file=sys.stderr)
                sys.exit(1)

            idx, line = splits
            try:
                idx = int(idx[1:])
            except ValueError:
                print(f"Invalid index specifier: {repr(idx)}", file=sys.stderr)
                sys.exit(1)

            return (idx, line)
        else:
            return None
