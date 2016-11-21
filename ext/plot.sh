rivet-mkhtml -c ext/bcalib.plot -o processplots --mc-errs \
    -m ".*elmu/os/.*" \
    $1/0.yoda:Title="data":LineColor=Black \
    $1/410000.yoda:Title="Pow+Py (nominal)"\
    $1/410001.yoda:Title="Pow+Py (radHi)"\
    $1/410002.yoda:Title="Pow+Py (radLo)"\
    $1/410003.yoda:Title="aMC+H++"\
    $1/410004.yoda:Title="Pow+H++":LineStyle=solid \
    $1/999999.yoda:Title="other":LineColor=Black


rivet-mkhtml -c ext/bcalib.plot -o flavorplots --mc-errs \
    -m ".*elmu/os/.*/allJetFlavs/.*" \
    $1/0.yoda:Title="data":LineColor=Black \
    $1/410000_light.yoda:Title="light" \
    $1/410000_charm.yoda:Title="charm" \
    $1/410000_bottom.yoda:Title="bottom"
