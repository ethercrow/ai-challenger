import React from 'react';

import ConsoleStore from '../stores/ConsoleStore';
import ConsoleActions from '../actions/ConsoleActions';

import ConsoleLogLine from './ConsoleLogLine.react';
import ConsoleCurrentLine from './ConsoleCurrentLine.react';

class Console extends React.Component {
    constructor(props) {
        super(props);
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        ConsoleStore.onChange(this._onChange);
        document.addEventListener('keypress', this._onKeyPress);
        document.addEventListener('keydown', this._onKeyDown);
    }
    
    componentWillUnmount() {
        ConsoleStore.off(this._onChange);
        document.removeEventListener('keydown', this._onKeyDown);
    }
    
    render() {
        var consoleLines = this.state.history.map(function(line, lineIndex) {
            return (
                <ConsoleLogLine key={lineIndex} string={line}/>
            );
        });
        
        return (
            <div id="console">
                {consoleLines}
            
                <ConsoleCurrentLine prompt={this.state.prompt} string={this.state.current_line}/>
            </div>
        );
    }
    
    _onKeyPress(event) {
        // Adapt to different browsers
        let code = 0;
        if(event.keyCode != undefined && event.keyCode != 0) {
            code = event.keyCode;
        } else {
            code = event.charCode;
        }
        
        switch(code) {
        case 8:
            // Do nothing. See _onKeyDown().
            break;
        
        case 13:
            ConsoleActions.consoleEnterTyped();
            break;
        
        default:
            ConsoleActions.consoleCharacterTyped(String.fromCharCode(code));
        }
    }
    
    _onKeyDown(event) {
        // Chrome specific hack!
        // It doesn't generate keypress events on backspace
        // So we process it in keydown handler
        let code = 0;
        if(event.keyCode != undefined && event.keyCode != 0) {
            code = event.keyCode;
        } else {
            code = event.charCode;
        }
        
        if(code == 8) {
            ConsoleActions.consoleBackspaceTyped();
        }
    }
    
    _resolveState() {
        return {
            history: ConsoleStore.getHistory(),
            prompt: ConsoleStore.getPrompt(),
            current_line: ConsoleStore.getCurrentLine()
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default Console;
