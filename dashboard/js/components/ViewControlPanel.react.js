import React from 'react';
import PlayPauseButton from './PlayPauseButton.react';
import ProgressBar from './ProgressBar.react';
import TurnsCounter from './TurnsCounter.react';
import LogButton from './LogButton.react';

class ViewControlPanel extends React.Component {
    render() {
        return (
            <div id="control-panel">
                <PlayPauseButton/>
                <ProgressBar/>
                <TurnsCounter/>
                <LogButton log_url={this.props.log_url}/>
            </div>
        );
    }
};

export default ViewControlPanel;
