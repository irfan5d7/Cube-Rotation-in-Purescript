var obj = {};


exports["get"] = function(key) {
    return function(){
        return obj[key];
    }
}

exports["set"] = function(key)
{
    return function(value)
    {
        return function(){
            obj[key] = value
        }
    }    
}

exports.addEventListener = function (ctx){
    return function (eventType) {
        return function(callback) {
            function eventHandler(e) {
                callback(e)();
            }
            canvas.addEventListener(eventType, eventHandler);
            return function () {}
        }
    }
}

exports["setVertex"]= function(vertex){
    return function(){
        obj["vertex"] = vertex;
    }
}

exports["getVertex"]= function(){
    return function(){
        console.log("getting", obj["vertex"])
        return obj["vertex"];
    }
}