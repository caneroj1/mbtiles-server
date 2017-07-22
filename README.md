# mbtiles-server
This server uses the [mbtiles](https://github.com/caneroj1/mbtiles) package to manage interacting with an mbtiles database.

## Usage
Run `stack exec mbtiles-server-exe`.

Then, send requests to `localhost:3000/tiles/{z}/{x}/{y}`.
