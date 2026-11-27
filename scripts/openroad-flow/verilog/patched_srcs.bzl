"""Hermetic source-patching build rule producing a `.srcjar` for `chisel_library`.

Replaces the custom `xs_source` repository rule: patching runs as a normal
cached Bazel build action, the checkout is never written, and standard
workspace targets (`//:xiangshan`) stay completely unpatched.
"""

def _patched_srcjar_impl(ctx):
    out = ctx.actions.declare_file(ctx.label.name + ".srcjar")
    strip = ctx.attr.strip_prefix
    script = ctx.actions.declare_file(ctx.label.name + "_make_srcjar.py")
    ctx.actions.write(
        output = script,
        content = """\
import os, shutil, subprocess, sys, tempfile, zipfile

out_jar = os.path.abspath(sys.argv[1])
strip = sys.argv[2]
n_patches = int(sys.argv[3])
patches = [os.path.abspath(p) for p in sys.argv[4 : 4 + n_patches]]
srcs = sys.argv[4 + n_patches :]

with tempfile.TemporaryDirectory() as work:
    for s in srcs:
        rel = s
        if strip and rel.startswith(strip + "/"):
            rel = rel[len(strip) + 1 :]
        dst = os.path.join(work, rel)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(s, dst)
    for p in patches:
        r = subprocess.run(["patch", "-p1", "-s", "-i", p], cwd=work, capture_output=True, text=True)
        if r.returncode != 0:
            sys.stderr.write(f"patch {p} failed:\\n{r.stdout}\\n{r.stderr}\\n")
            sys.exit(r.returncode)
    all_files = []
    for root, _, files in os.walk(work):
        for fn in files:
            if fn.endswith(".scala") or fn.endswith(".java"):
                full = os.path.join(root, fn)
                all_files.append((os.path.relpath(full, work), full))
    all_files.sort()
    with zipfile.ZipFile(out_jar, "w", compression=zipfile.ZIP_DEFLATED) as zf:
        for rel, full in all_files:
            zi = zipfile.ZipInfo(rel, date_time=(2020, 1, 1, 0, 0, 0))
            zi.compress_type = zipfile.ZIP_DEFLATED
            with open(full, "rb") as fh:
                zf.writestr(zi, fh.read())
""",
    )
    args = ctx.actions.args()
    args.add(out)
    args.add(strip)
    args.add(str(len(ctx.files.patches)))
    args.add_all(ctx.files.patches)
    args.add_all(ctx.files.srcs)
    ctx.actions.run(
        executable = ctx.executable._python,
        arguments = [script.path, args],
        inputs = depset([script] + ctx.files.patches + ctx.files.srcs),
        outputs = [out],
        mnemonic = "PatchedSrcJar",
        progress_message = "Building patched srcjar %s" % ctx.label,
    )
    return [DefaultInfo(files = depset([out]))]

patched_srcjar = rule(
    implementation = _patched_srcjar_impl,
    attrs = {
        "patches": attr.label_list(allow_files = [".patch"]),
        "srcs": attr.label_list(allow_files = True, mandatory = True),
        "strip_prefix": attr.string(default = ""),
        "_python": attr.label(
            default = Label("@rules_python//python/bin:python"),
            executable = True,
            cfg = "exec",
        ),
    },
)
