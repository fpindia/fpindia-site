window.onload = function () {
    document.getElementById('burger-button').onclick = function () {
        document.getElementById('burger-menu').classList.remove('translate-x-full');
    };
    document.getElementsByClassName('burger-close').onclick = function () {
        document.getElementById('burger-menu').classList.add('translate-x-full');
    };
}
