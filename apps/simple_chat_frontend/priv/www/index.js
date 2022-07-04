document.addEventListener('DOMContentLoaded', () => {
    const url = 'http://127.0.0.1:8080/simple_chat/web:do';

    function getCookie(name) {
        let cookieValue = null;
        if (document.cookie && document.cookie !== '') {
            const cookies = document.cookie.split(';');
            for (let i = 0; i < cookies.length; i++) {
                const cookie = cookies[i].trim();
                // Does this cookie string begin with the name we want?
                if (cookie.substring(0, name.length + 1) === (name + '=')) {
                    cookieValue = decodeURIComponent(cookie.substring(name.length + 1));
                    break;
                }
            }
        }
        return cookieValue;
    }
    const csrftoken = getCookie('csrftoken');

    fetch(url, {
        method: 'GET',
        headers: {
            'Content-type': 'application/json; charset=UTF-8',
        }
    })
        .then(response => response.json()) // convert to json
        .then(data => {
            if (close(data)) {
                addMessages(data.messages);
            }
        })
        .catch(err => console.log(err));   // catch errors

    if ('WebSocket' in window) {
        console.log('Open WebSocket...');
        var ws = new WebSocket('ws://127.0.0.1:8081');
    }

    ws.onopen = function () {
        console.log('Web Socket is connected. You can send data by send() method.');
    }

    document.getElementById('send').onclick = (e) => send();

    function send() {
        var message = document.getElementById('message').value;
        var message = {
            id: this.state,
            text: message
        };
        document.getElementById('message').value = '';
        ws.send(JSON.stringify(message));
    }

    ws.onmessage = function (evt) {
        addMessages(evt.data);
    }

    ws.onclose = function () {
        console.log('Connection closed');
    };

    document.getElementById('signup').onclick = (e) => signup(e);

    function signup(e) {
        e.preventDefault();
        var name = document.getElementById('nickname').value;
        var pass = document.getElementById('psw').value;
        var cPass = document.getElementById('cPsw').value;
        let data = {
            login: 'login',
            name: name,
            password: pass,
            confirmPassword: cPass
        }

        fetch(url, {
            method: 'POST',
            body: JSON.stringify(data),
            headers: {
                'Content-type': 'application/json; charset=UTF-8',
                'X-CSRFToken': csrftoken
            }
        })
            .then(response => response.json()) // convert to json
            .then(data => getMessages(data))
            .catch(err => console.log(err));   // catch errors
    }

    function getMessages(data) {
        if (close(data)) {
            fetch(url, {
                method: 'GET',
                headers: {
                    'Content-type': 'application/json; charset=UTF-8',
                }
            })
                .then(response => response.json()) // convert to json
                .then(info => addMessages(info.messages))
                .catch(err => console.log(err));   // catch errors
        }
    }

    function close(data) {
        if (data.login === 'false') {
            return false;
        }
        this.state = data.login
        var modal = document.getElementById('myModal');
        var main = document.querySelector('.main');
        var bottom = document.querySelector('.bottom');
        modal.style.display = 'none';
        main.style.filter = 'none';
        bottom.style.filter = 'none';
        return true;
    }

    function addMessages(data) {
        if (Array.isArray(data)) {
            for (var i = 0; i < data.length; i++) {
                if (data[i].length) {
                    data[i].forEach(element => showMessages(element));
                }
            }
        } else {
            showMessages(data);
        }
    }

    function showMessages(messages) {
        if (messages !== '' || messages.length) {
            var jsonData = JSON.parse(messages);
            var main = document.getElementsByClassName('main')[0];
            var div = document.createElement('div');
            var p = document.createElement('p');
            p.setAttribute('class', 'message-text');
            p.textContent = jsonData.text;
            var time = document.createElement('span');
            var icon = document.createElement('span');
            if (this.state[1] === jsonData.name) {
                div.setAttribute('class', 'container darker');
                time.setAttribute('class', 'time-left');
                icon.setAttribute('class', 'icon-right');
            } else {
                div.setAttribute('class', 'container');
                time.setAttribute('class', 'time-right');
                icon.setAttribute('class', 'icon-left');
            }
            time.textContent = jsonData.time[0] + ':' + minutes_with_leading_zeros(jsonData.time[1]);
            icon.textContent = jsonData.name;
            div.appendChild(icon);
            div.appendChild(p);
            div.appendChild(time);
            main.appendChild(div);
        }
    }

    function minutes_with_leading_zeros(minutes) {
        return (minutes < 10 ? '0' : '') + minutes;
    }
});