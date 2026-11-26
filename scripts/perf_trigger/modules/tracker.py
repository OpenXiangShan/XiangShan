import logging


class Tracker:
    def __init__(self, total: int, keys: list[str], with_keys: bool = True) -> None:
        self.total = total
        self.currents = {key: 0 for key in keys}
        self.with_keys = with_keys

    def step(self, key: str, n: int = 1) -> None:
        self.currents[key] += n

    def reset(self) -> None:
        for key in self.currents:
            self.currents[key] = 0

    def format_progress(self) -> str:
        if self.with_keys:
            return (
                ",".join(f"{key}:{value}" for key, value in self.currents.items())
                + f"/{self.total}"
            )
        else:
            return (
                ",".join(str(value) for value in self.currents.values())
                + f"/{self.total}"
            )

    def debug(self, msg: str, *args: object) -> None:
        logging.debug("[%s] " + msg, self.format_progress(), *args)

    def info(self, msg: str, *args: object) -> None:
        logging.info("[%s] " + msg, self.format_progress(), *args)

    def warning(self, msg: str, *args: object) -> None:
        logging.warning("[%s] " + msg, self.format_progress(), *args)

    def error(self, msg: str, *args: object) -> None:
        logging.error("[%s] " + msg, self.format_progress(), *args)

    def critical(self, msg: str, *args: object) -> None:
        logging.critical("[%s] " + msg, self.format_progress(), *args)
