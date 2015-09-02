import Constants from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';

export default {
    consoleCharacterTyped(character) {
        Dispatcher.dispatch({ 
            type: Constants.CONSOLE_CHARACTER_TYPED,
            data: character 
        });
    },
    
    consoleBackspaceTyped() {
        Dispatcher.dispatch({ type: Constants.CONSOLE_BACKSPACE_TYPED });
    },
    
    consoleEnterTyped() {
        Dispatcher.dispatch({ type: Constants.CONSOLE_ENTER_TYPED });
    },
    
    writeLine(str) {
        Dispatcher.dispatch({ 
            type: Constants.CONSOLE_WRITE_LINE,
            data: str
        });
    }
};
