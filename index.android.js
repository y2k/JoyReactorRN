import React from 'react';
import { AppRegistry } from 'react-native';
import App from './build/ui'

const app = () => <App />
AppRegistry.registerComponent('joyreact', () => app);