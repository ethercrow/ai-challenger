import React from 'react';
import ConsoleLogLine from './ConsoleLogLine.react'
import ConsoleCurrentLine from './ConsoleCurrentLine.react'

class Console extends React.Component {
    render() {
        return (
            <div id="console">
                <ConsoleLogLine>Welcome to AI Challenger v0.0.1!</ConsoleLogLine>
                <ConsoleLogLine>Please type 'help' for the list of available commands.</ConsoleLogLine>
            
                <ConsoleCurrentLine prompt="root@ai-challenger$&nbsp;"></ConsoleCurrentLine>
            </div>
        );
    }
};

export default Console;
