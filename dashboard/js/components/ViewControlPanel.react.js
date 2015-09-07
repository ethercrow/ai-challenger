import React from 'react';
import PlayPauseButton from './PlayPauseButton.react';
import ProgressBar from './ProgressBar.react';
import TurnsCounter from './TurnsCounter.react';

class ViewControlPanel extends React.Component {
    render() {
        return (
            <div id="control-panel">
                <PlayPauseButton/>
                <ProgressBar/>
                <TurnsCounter/>
            </div>
        );
    }
};

export default ViewControlPanel;
