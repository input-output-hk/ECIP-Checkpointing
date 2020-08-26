#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p "python3.withPackages(ps: [])"

"""
This script is aimed to be used to mock the Blockchain RPC endpoint.

You can use it to debug the morpho node RPC requests.

NOTE: you'll need nix-shell installed on your system to use this python script.
"""

from http.server import HTTPServer, BaseHTTPRequestHandler
import json
import sys

resp_last_block = '''
{
"jsonrpc": "2.0",
"result": {
    "number": "0x2A",
    "hash": "0xee31cf07cff0ddd68b709c9a3d59e4699ee3ba44db209eac4792123bc887588d"
},
"id": 1
}
'''

resp_checkpoint = '''
{
"jsonrpc": "2.0",
"result": {
    "status": 1
},
"id": 1
}
'''

class BlockchainRPCHandler(BaseHTTPRequestHandler):
    def _set_headers(self):
        self.send_response(200)
        self.send_header("Content-type", "application/json")
        self.end_headers()

    def do_HEAD(self):
        self._set_headers()

    def do_GET(self):
        self._set_headers()
        self.wfile.write("ERROR: request should be POST, not GET".encode("utf8"))

    def do_POST(self):
        self._set_headers()
        content_length = int(self.headers['Content-Length'])
        try:
            request_data = json.loads(self.rfile.read(content_length).decode("utf8"))
            if request_data['method'] == "checkpointing_pushCheckpoint":
                self.wfile.write(resp_checkpoint.encode("utf8"))
                print("Checkpoint push request:")
                print(request_data)
            elif request_data['method'] == "checkpointing_getLatestBlock":
                self.wfile.write(resp_last_block.encode("utf8"))
                print("Get lastest block request:")
                print(request_data)
            else:
                str_resp = "Cannot reconise " + request_data['method'] + " method."
                self.wfile.write(str_resp.encode("utf8"))
                print(str_resp)
        except json.JSONDecodeError:
            print("Cannot decode JSON")
            self.wfile.write("Cannot decode json".encode("utf8"))
        except:
            e = sys.exc_info()[0]
            str_resp = format("Error: %s" % e)
            self.wfile.write(str_resp.encode("utf8"))

def startServer():
    httpd = HTTPServer(("localhost",8546), BlockchainRPCHandler)
    print(f"Starting server on localhost:8546")
    httpd.serve_forever()
    return True

if __name__ == '__main__':
   startServer()
