set -e
set -x

cd $NOOP_HOME

for module in {XSTop,XSTile,XSCore,Frontend,CtrlBlock,ExuBlock,ExuBlock_1,MemBlock,HuanCun,HuanCun_1}; do
  python3 scripts/parser.py $module --config DefaultConfig --prefix bosc_ --sram-replace --xs-home $(pwd) --mbist-scan-replace > generate_$module.log
  mv generate_$module.log bosc_${module}-Release*
done

