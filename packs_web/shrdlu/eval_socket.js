var MIN_RECONNECT_DELAY = 10000;
var MAX_RECONNECT_DELAY = 30000;

(function($) {

	if (typeof window.JSCtrl === "undefined") {

		function JSCtrl(url) {
			var defaultURL = window.location.protocol.replace("http", "ws") + // gets 'ws' or 'wss:'
				"//" + window.location.host + ":14302/swish/jseval_ws";
			this.url = url || defaultURL;
			DEBUGGING = false;
			reconnectsAvail = 10;
			reconnectScheduled = false;
			objToName = new Map();
			nameToObj = new Map();
			simpleMode = true;
			simpleMode2 = true;
		}

		window.JSCtrl = JSCtrl;
		jsev = new JSCtrl();
		game = window.theA4Game;

		function loadjsfile(filename, datamain) {
			/*$(jQuery).getScript( filename )
			  .done(function( script, textStatus ) {
			    console.log( textStatus + " " + filename  );
			  })
			  .fail(function( jqxhr, settings, exception ) {
				debugger;    
			});*/
			var fileref = document.createElement('script')
			fileref.setAttribute("type", "text/javascript")
			fileref.setAttribute("src", filename)
			fileref.setAttribute("data-main", datamain)
			if (typeof fileref != "undefined")
				document.getElementsByTagName("head")[0].appendChild(fileref);
		}

		//loadjsfile("https://code.jquery.com/jquery-3.6.0.min.js")// crossorigin="anonymous">
		//loadjsfile("/node_modules/requirejs/require.js","/swish/js/swish");
		//loadjsfile("/swish/node_modules/reflect-metadata/Reflect.js");
		// loadjsfile("/swish/node_modules/class-transformer/cjs/index.js");
		loadjsfile("eval_socket_hydrate.js");



		JSCtrl.prototype.scheduleReconnect = function() {
			if (reconnectScheduled) return;
			reconnectScheduled = true;
			if (reconnectsAvail > 0) {
				reconnectsAvail--;
				setTimeout(function() {
					setTimeout(function() {
						console.warn("Reconnection to remote JSCtrl on " + jsev.url);
						reconnectScheduled = false;
						jsev.connect();
					}, MIN_RECONNECT_DELAY);
				}, MAX_RECONNECT_DELAY);
			}
		}

		JSCtrl.prototype.getClassName = function(obj) {
			if (obj == null) return "@null";
			if (typeof obj === "undefined") return "@undefined";
			var funcNameRegex = /function (.{1,})\(/;
			var results = (funcNameRegex).exec((obj).constructor.toString());
			return (results && results.length > 1) ? results[1] : "";
		};

		JSCtrl.prototype.connect = function() {

			try {
				//this.url = "wss://echo.websocket.org";
				//this.url = "wss://logicmoo.org:14302/swish/jseval_ws";
				var socket = new WebSocket(this.url);
				this.socket = socket;

				socket.onopen = function(e) {
					reconnectsAvail = 10;
					console.log("JSCtrl: " + "[open] Connection established");
					var sessionId = /SESS\w*ID=([^;]+)/i.test(document.cookie) ? RegExp.$1 : false;
					socket.send("sessionId=" + sessionId);
				};

				socket.onmessage = function(message) {
					console.log("JSCtrl: " + `[message] Data received from server: ${message.data}`);
					try {
						//debugger;
						var messageData = message.data;
						var evalThis = true;

						if (messageData.startsWith("+")) {
							messageData = messageData.substring(1);
							evalThis = true;
						}
						if (messageData.startsWith("-")) {
							messageData = messageData.substring(1);
							evalThis = false;
						}
						if (evalThis) {
							var res = window.eval(messageData);

							var reply = null;
							res = jsev.typeIfy(res);
							var html = jsev.maybeHtml(res, 0);
							if (jsev.isHtmlish(html)) {
								reply = html;
							} else {
								// reply = forestify_aka_decycle(res);
								reply = jsev.stringfyAsJson(res);
							}

							if (DEBUGGING) { // for debugging
								if (typeof reply === 'undefined') {} else {
									if (typeof reply.length != 'undefined' && reply.length < 3000) {
										console.log("JSCtrl: " + `[reply] Replying with: ${reply}`);
									} else if (typeof reply.substring != 'undefined') {
										var some = reply.substring(0, 100);
										console.log("JSCtrl: " + `[reply] Replying.length with: ${some}...${reply.length}`);
									} else {
										console.log("JSCtrl: " + `[reply] Replying with: ${reply}`);
									}
								}
							}
							if (typeof reply === "string") {
								socket.send(reply);
							} else {
								socket.send(JSON.stringify(reply));
							}
						}
					} catch (e) {
						DEBUGGING = true;
						console.error(e);
						//debugger;
						socket.send(JSON.stringify({
							"error": {
								"message": e.message,
								"trace": e.trace,
								"original": message
							}
						}))
					}
				}

				socket.onclose = function(event) {
					console.warn(event);
					if (event.wasClean) {
						console.log("JSCtrl: " + `[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
						reconnectsAvail = 10;
					} else {
						// e.g. server process killed or network down
						// event.code is usually 1006 in this case
						console.log("JSCtrl: " + `[close] Connection died, code=${event.code} reason=${event.reason}`);
					}
					jsev.scheduleReconnect();
				};

				socket.onerror = function(error) {
					console.warn(error);
					if (error != null && error.message != undefined) {
						console.log("JSCtrl: " + `[error] ${error.message}`);
					}
					jsev.scheduleReconnect();
				};



			} catch (e) {
				jsev.scheduleReconnect();
			}

		}


		JSCtrl.prototype.loadDocument = function(from) {
			var xmlhttp = new XMLHttpRequest();
			xmlhttp.overrideMimeType("text/xml");
			xmlhttp.open("GET", from, false);
			xmlhttp.send();
			return xmlhttp.responseXML.documentElement;
		}


		// inspect the return result of maybeHtml
		JSCtrl.prototype.isHtmlish = function(value, depth) {
			if (typeof value === "string") return true;
			if (typeof value === "undefined") return false;
			//if (typeof value === "DefinedRef") return true;
			if (value == false) return false;
			//if (typeof value.toJSON === "function") return true;
			return false;
		}

		// return +<some>html</some> if value contains .outerHTML
		JSCtrl.prototype.maybeHtml = function(value0, depth) {
			if (!(value0 != null)) {
				return false;
			}
			if (typeof value0 === "undefined") {
				return false;
			}
			var value = jsev.typeIfy(value0);

			if (typeof value.outerHTML === 'function') {
				return "+" + value.outerHTML().trim();
			}

			if (simpleMode2) return false;

			if (depth > 2 && typeof value.saveToXML === 'function') {
				return "+" + value.saveToXML().trim();
			}

			if (depth > 1 && typeof value.savePropertiesToXML === 'function') {
			    return "+" + value.savePropertiesToXML(theA4Game).trim();

			}
			if (simpleMode) return false;
			try {
				if (depth < 3) return JSON.stringify(value);
				return "<@ " + JSON.stringify(value) + " @>";
			} catch (e) {
				// ignore
			}
			return false;
		}

		JSCtrl.prototype.stringfyAsJson = function(value) {
			value = jsev.typeIfy(value);
			return JSON.stringify(value, jsev.refReplacer(value));
		}

		JSCtrl.prototype.refReplacer = function() {
			let m = new Map(),
				v = new Map(),
				init = null;

			return function(field, value) {

				if (value == null) return null;
				if (typeof value === "undefined") return value;
				if (typeof value === "string") return value;
				 
				value = jsev.typeIfy(value);

				let p = m.get(this) + (Array.isArray(this) ? `[${field}]` : '.' + field);
				let isComplex = value === Object(value)

				if (isComplex) m.set(value, p);

				let pp = v.get(value) || '';
				let path = p.replace(/undefined\.\.?/, '');
				let val = pp ? `#REF:${pp[0]=='[' ? '$':'$.'}${pp}` : value;

				!init ? (init = value) : (val === init ? val = "#REF:$" : 0);
				if (!pp && isComplex) v.set(value, path);

				if (true) {
					var html = jsev.maybeHtml(val, 0);
					if (jsev.isHtmlish(html)) return html;
				}

				return val;
			}
		}
	}

	jsev.connect();
	JSCtrl.prototype.typeIfy = function(value) {
		if (simpleMode) return value;
		if (value == null) return null;
		let isComplex = value === Object(value);

		// if(!isComplex) return value;
		try {
			if (typeof value.getClassName != "undefined") {
				return value;
			}
			var cn = jsev.getClassName(value);
			value["getClassName"] = cn;
			value.getClassName = cn;
			//  var obj2 = { getClassName: cn };
			//  value = {...value, ...obj2 };
			//  Object.assign(value,obj2);
		} catch (e) {
			console.error(e);
			// ignored
		}
		return value;
	}


	JSCtrl.prototype.parseRefJSON = function(json) {
		let objToPath = new Map();
		let pathToObj = new Map();
		let o = JSON.parse(json);

		let traverse = (parent, field) => {
			let obj = parent;
			let path = '#REF:$';

			if (field !== undefined) {
				obj = parent[field];
				path = objToPath.get(parent) + (Array.isArray(parent) ? `[${field}]` : `${field?'.'+field:''}`);
			}

			objToPath.set(obj, path);
			pathToObj.set(path, obj);

			let ref = pathToObj.get(obj);
			if (ref) parent[field] = ref;

			for (let f in obj)
				if (obj === Object(obj)) traverse(obj, f);
		}

		traverse(o);
		return o;
	}


})(window)