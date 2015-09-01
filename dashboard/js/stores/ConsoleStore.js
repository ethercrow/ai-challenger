import { EventEmitter } from 'events';
import ActionTypes from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';

const CONSOLE_CHANGE_EVENT = 'console_change';

const initialData = {
    history: [
        "Welcome to AI Challenger v0.0.1!",
        "Please type 'help' for the list of available commands."
    ],
    
    prompt: "root@ai-challenger$ ",
    current_line: ""
};
let data = initialData;

class ConsoleStore extends EventEmitter {
    emitChange() {
        this.emit(CONSOLE_CHANGE_EVENT);
    }
    
    onChange(callback) {
        this.on(CONSOLE_CHANGE_EVENT, callback);
    }
    
    off(callback) {
        this.removeListener(CONSOLE_CHANGE_EVENT, callback);
    }
    
    addCharacterToCurrentLine(character) {
        data.current_line = data.current_line + character;
    }
    
    removeCharacterFromCurrentLine() {
        data.current_line = data.current_line.substring(0, data.current_line.length - 1);
    }
    
    executeCurrentLine() {
        data.history.push(data.prompt + data.current_line);
        data.current_line = '';
    }
    
    getHistory() {
        return data.history;
    }
    
    getPrompt() {
        return data.prompt;
    }
    
    getCurrentLine() {
        return data.current_line;
    }
};

var _ConsoleStore = new ConsoleStore();
export default _ConsoleStore;

_ConsoleStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
    case ActionTypes.CONSOLE_CHARACTER_TYPED:
        _ConsoleStore.addCharacterToCurrentLine(action.data);
        _ConsoleStore.emitChange();
        break;
    
    case ActionTypes.CONSOLE_BACKSPACE_TYPED:
        _ConsoleStore.removeCharacterFromCurrentLine();
        _ConsoleStore.emitChange();
        break;
    
    case ActionTypes.CONSOLE_ENTER_TYPED:
        _ConsoleStore.executeCurrentLine();
        _ConsoleStore.emitChange();
        break;
    
    default:
    }
});
