var lupo = lupo || {
  createResponse: function() {
    var challenge = document.getElementById("login-challenge"),
        passInput = document.getElementById("login-pass-input"),
        passCrypto = document.getElementById("login-pass-crypto");
    passCrypto.value = CryptoJS.SHA1(challenge.value + CryptoJS.SHA1(passInput.value));
  }
};
