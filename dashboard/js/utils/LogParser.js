function _tokenize(str) {
    return str.split(' ').reduce((acc, token) => {
        if(token != "") {
            return acc + [token];
        } else {
            return acc;
        }
    }, []);
}

function _isMapLine(str) {
    return str[0] == '-' || str[0] == '1' || str[0] == '2';
}

function _parseMapLine(str) {
    let line = [];
    for(let i = 0; i < str.length; i++) {
        switch(str[i]) {
        case '-':
            line.push(0);
            break;
        
        case '1':
            line.push(1);
            break;
        
        case '2':
            line.push(2);
            break;
        
        default:
            console.error('ERROR: Unknown symbol encountered during the map parsing!');
            line.push(undefined);
        }
    }
    return line;
}

function _parseMap(str) {
    let map = [];
    
    const commands = str.split('\n');
    let parse_map = false;
    commands.forEach((cmd) => {
        let tokens = _tokenize(cmd);
        
        switch(tokens[0]) {
        case 'W':
            parse_map = true;
            map = [];
            break;
        
        case 'O':
            parse_map = false;
            break;
        
        default:
            if(parse_map && _isMapLine(tokens)) {
                map.push(_parseMapLine(tokens));
            }
        }
    });
    
    return map;
}

function _parsePlayerCommands(str) {
    let result = [[], []];
    
    const commands = str.split('\n');
    let current_player = -1;
    commands.forEach((cmd) => {
        let tokens = _tokenize(cmd);
        
        switch(tokens[0]) {
        case 'O':
            current_player = Number.parseInt(tokens[1]);
            break;
        
        case 'C':
            if(current_player != -1) {
                result[current_player - 1].push({
                    x: Number.parseInt(tokens[1]),
                    y: Number.parseInt(tokens[2])
                });
            }
            break;
        
        default:
        }
    });
    
    return result;
}

export default {
    separateTurns(log) {
        return log.split('.').reduce((acc, token) => {
            if(token.trim().length != 0) {
                acc.push(token);
                return acc;
            } else {
                return acc;
            }
        }, []);
    },
    
    parseTurn(str) {
        return {
            map: _parseMap(str),
            commands: _parsePlayerCommands(str)
        };
    }
};
