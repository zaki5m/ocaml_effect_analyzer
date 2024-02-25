import cytoscape from 'cytoscape';

const elements: cytoscape.ElementDefinition[] = [
    { data: { id: 'a' } },
    { data: { id: 'b' } },
    { data: { id: 'ab', source: 'a', target: 'b' } }
];

const style: cytoscape.Stylesheet[] = [
    {
        selector: 'node',
        style: {
            'background-color': '#666',
            'label': 'data(id)'
        }
    },
    {
        selector: 'edge',
        style: {
            'width': 3,
            'line-color': '#ccc',
            'target-arrow-color': '#ccc',
            'target-arrow-shape': 'triangle'
        }
    }
];

const layout = {
    name: 'grid',
    rows: 1
};

cytoscape({
    container: document.getElementById('cy'),
    elements,
    style,
    layout
});
