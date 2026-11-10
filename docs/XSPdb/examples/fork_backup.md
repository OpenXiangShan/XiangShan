# Fork Backup Examples

```text
# Enable fork backup: window 5s, dump to ./, log to ./fork_backup.log
xfork_backup_on 5.0 . ./fork_backup.log

# Set an xbreak and run
xbreak SimTop_top.SimTop.cpu.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_commit_pc == 0x80000000
xstep 100000

# Inspect status
xfork_backup_status

# Disable when done
xfork_backup_off
```
