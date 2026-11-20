"""XiangShan's own sources, read from the checkout, with the flow's patches.

new_local_repository reads a directory in place and cannot patch it. The
flow carries patches on XiangShan's RTL (verilog/patches, each header says
why) that must not touch the checkout, so this rule copies the
directories it is asked for out of the checkout into the repository,
applies the patches to the copy, and watches the originals: an edit in
the checkout re-runs the rule. The checkout itself is never written.
"""

def _xs_source_impl(rctx):
    # The checkout, relative to this module's root (scripts/openroad-flow).
    root = str(rctx.workspace_root) + "/" + rctx.attr.path
    for d in rctx.attr.dirs:
        src = rctx.path(root + "/" + d)
        if not src.exists:
            fail("xs_source: %s does not exist; run `make init` in the XiangShan checkout" % src)
        rctx.watch_tree(src)
        parent = d.rpartition("/")[0]
        if parent:
            rctx.execute(["mkdir", "-p", parent])
        result = rctx.execute(["cp", "-R", str(src), d])
        if result.return_code != 0:
            fail("xs_source: copying %s failed: %s" % (src, result.stderr))
    for patch in rctx.attr.patches:
        rctx.patch(patch, strip = 1)
    rctx.symlink(rctx.attr.build_file, "BUILD.bazel")

xs_source = repository_rule(
    implementation = _xs_source_impl,
    attrs = {
        "build_file": attr.label(mandatory = True, allow_single_file = True),
        "dirs": attr.string_list(
            mandatory = True,
            doc = "Directories under path to copy, relative to it.",
        ),
        "patches": attr.label_list(
            allow_files = [".patch"],
            doc = "Applied to the copy with strip 1, in order.",
        ),
        "path": attr.string(
            mandatory = True,
            doc = "The checkout directory, relative to this module's root.",
        ),
    },
    doc = "A copy of directories of the XiangShan checkout, patched.",
)
