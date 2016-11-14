rivet-mkhtml -c ext/bcalib.plot -o processplots --mc-errs \
    -m ".*elmu/os/.*/allJetFlavs/.*" \
    $1/0.yoda:LineColor=Black:Title="data" \
    $1/410000.yoda \
    $1/410001.yoda \
    $1/410002.yoda \
    $1/410003.yoda \
    $1/410004.yoda:LineStyle=solid \
    $1/999999.yoda:Title=other:LineColor=Black


rivet-mkhtml -c ext/bcalib.plot -o flavorplots --mc-errs \
    -m ".*elmu/os/.*/allJetFlavs/.*" \
    $1/0.yoda:LineColor=Black:Title="data" \
    $1/410000_light.yoda:Title="light" \
    $1/410000_charm.yoda:Title="charm" \
    $1/410000_bottom.yoda:Title="bottom"
