import cytoscape from 'cytoscape';
import elements from './elements.json';

const style: cytoscape.Stylesheet[] = [
    {
        selector: 'node[label = "Effect"]',
        style: {
            'background-color': '#666',
            'label': 'data(name)'
        }
    },
    {
        selector: 'edge',
        style: {
            'width': 3,
            'line-color': '#ccc',
            'target-arrow-color': '#ccc',
            'target-arrow-shape': 'triangle',
            'curve-style': 'bezier'
        }
    }
];

const layout = {
    name: 'grid',
};

cytoscape({
    container: document.getElementById('cy'),
    elements,
    style,
    layout
});
