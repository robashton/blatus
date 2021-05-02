// These are not used
exports.seed =  function() { return null; };
exports.random = function(tuple) {
  return function(seed) { return tuple(0.0)(seed); }; // no-op, we don't need numbers on the client
}
