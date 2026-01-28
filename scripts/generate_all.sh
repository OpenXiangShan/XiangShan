set -e
set -x

cd $NOOP_HOME

for module in Region
do
  python3 scripts/parser.py $module --config TLConfig --prefix bosc_ --sram-replace --xs-home $(pwd) > generate_$module.log
  mv generate_$module.log bosc_${module}-Release*
done

