mkdir release
mkdir release/data
mkdir release/data/fonts

cp ".stack-work/install/x86_64-osx/lts-8.11/8.0.2/bin/visor-app" "release/visor"
cp "data/Melee" "release/data/Melee"
cp "data/fonts/Roboto-Regular.ttf" "release/data/fonts/Roboto-Regular.ttf"

zip -r visor.zip release
rm -rf release
