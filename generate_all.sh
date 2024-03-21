set -e
set -x

cd $NOOP_HOME

for module in  XSTop SimTop
do
  python3 parser.py $module --config DefaultConfig --prefix bosc_ --sram-replace --xs-home $(pwd) > generate_$module.log
  mv generate_$module.log bosc_${module}-Release*
done

