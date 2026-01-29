import json
import os


class UiConfigController:
    def __init__(self, app, config_path: str | None = None):
        self._app = app
        self._theme_names = []
        self._theme_index = 0
        self._config_path = config_path or os.path.expanduser("~/.xspdb/xui.json")

    @property
    def theme_names(self):
        return self._theme_names

    def init_themes(self) -> None:
        try:
            available = list(getattr(self._app, "available_themes", []))
        except Exception:
            available = []
        preferred = [
            "textual-dark",
            "textual-light",
            "nord",
            "gruvbox",
            "dracula",
            "monokai",
            "catppuccin-mocha",
            "catppuccin-latte",
            "rose-pine",
            "flexoki",
        ]
        self._theme_names = [t for t in preferred if t in available] or sorted(available)
        current = getattr(self._app, "current_theme", "")
        if current in self._theme_names:
            self._theme_index = self._theme_names.index(current)
        else:
            self._theme_index = 0

    def set_theme(self, name: str) -> None:
        try:
            self._app.theme = name
            self._app._log.log_line(f"[Theme] {name}", "info")
            if name in self._theme_names:
                self._theme_index = self._theme_names.index(name)
        except Exception:
            pass

    def toggle_theme(self) -> None:
        if self._theme_names and "textual-dark" in self._theme_names and "textual-light" in self._theme_names:
            current = getattr(self._app, "current_theme", "")
            target = "textual-light" if current == "textual-dark" else "textual-dark"
            self.set_theme(target)
            return
        try:
            self._app.dark = not self._app.dark
        except Exception:
            pass

    def cycle_theme(self) -> None:
        if not self._theme_names:
            return
        self._theme_index = (self._theme_index + 1) % len(self._theme_names)
        self.set_theme(self._theme_names[self._theme_index])

    def theme_list(self) -> None:
        if not self._theme_names:
            return
        self._app._log.log_line("[Themes] " + ", ".join(self._theme_names), "info")

    def load_config(self) -> bool:
        try:
            if not os.path.exists(self._config_path):
                return False
            with open(self._config_path, "r", encoding="utf-8") as f:
                data = json.load(f)
        except Exception:
            return False
        try:
            left = int(data.get("left_width", self._app._left_width))
            bottom = int(data.get("bottom_height", self._app._bottom_height))
            right = int(data.get("right_width", self._app._right_width))
            self._app._left_width = max(20, min(200, left))
            self._app._bottom_height = max(6, min(30, bottom))
            self._app._right_width = max(20, min(120, right))
        except Exception:
            pass
        theme = data.get("theme", "")
        if theme and theme in getattr(self._app, "available_themes", {}):
            self.set_theme(theme)
        self._app._apply_layout()
        return True

    def save_config(self) -> bool:
        theme = getattr(self._app, "current_theme", "")
        if hasattr(theme, "name"):
            theme = theme.name
        data = {
            "theme": str(theme or ""),
            "left_width": int(self._app._left_width),
            "bottom_height": int(self._app._bottom_height),
            "right_width": int(self._app._right_width),
        }
        try:
            os.makedirs(os.path.dirname(self._config_path), exist_ok=True)
            tmp_path = self._config_path + ".tmp"
            with open(tmp_path, "w", encoding="utf-8") as f:
                json.dump(data, f, indent=2, ensure_ascii=True)
            os.replace(tmp_path, self._config_path)
            return True
        except Exception:
            return False
