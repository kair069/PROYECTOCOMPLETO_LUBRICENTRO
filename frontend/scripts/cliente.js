document.addEventListener('DOMContentLoaded', () => {
    obtenerClientes();

    document.getElementById('formulario-cliente').addEventListener('submit', function (event) {
        event.preventDefault();
        const nombre = document.getElementById('nombre').value;
        const apellido = document.getElementById('apellido').value;
        const correo = document.getElementById('correo').value;
        const departamento = document.getElementById('departamento').value;
        const dni = document.getElementById('dni').value;
        const numeroDeTelefono = document.getElementById('numeroDeTelefono').value;
        const pais = document.getElementById('pais').value;
        const ubicacionDeVivienda = document.getElementById('ubicacionDeVivienda').value;
        const enlaceImagen = document.getElementById('enlaceImagen').value;

        crearCliente({ nombre, apellido, correo, departamento, dni, numeroDeTelefono, pais, ubicacionDeVivienda, enlaceImagen });
    });

    document.getElementById('formulario-editar').addEventListener('submit', function (event) {
        event.preventDefault();
        guardarEdicion();
    });
});

function obtenerClientes() {
    fetch('http://localhost:8081/clientes')
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(data => {
            mostrarClientes(data);
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

function mostrarClientes(clientes) {
    const clientesSection = document.getElementById('clientes');
    clientesSection.innerHTML = '';

    clientes.forEach(cliente => {
        const clienteElement = document.createElement('div');
        clienteElement.classList.add('cliente');
        clienteElement.innerHTML = `
            <h3>${cliente.nombre} ${cliente.apellido}</h3>
            <p>Correo: ${cliente.correo}</p>
            <p>Departamento: ${cliente.departamento}</p>
            <p>DNI: ${cliente.dni}</p>
            <p>Número de Teléfono: ${cliente.numeroDeTelefono}</p>
            <p>País: ${cliente.pais}</p>
            <p>Ubicación de Vivienda: ${cliente.ubicacionDeVivienda}</p>
            <img src="${cliente.enlaceImagen}" alt="Imagen del Cliente">
            <button onclick="editarCliente(${cliente.id})">Editar</button>
            <button onclick="eliminarCliente(${cliente.id})">Eliminar</button>
        `;
        clientesSection.appendChild(clienteElement);
    });
}

function crearCliente(cliente) {
    fetch('http://localhost:8081/clientes', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(cliente),
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            obtenerClientes();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

function editarCliente(clienteId) {
    fetch(`http://localhost:8081/clientes/${clienteId}`)
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(cliente => {
            document.getElementById('editId').value = cliente.id;
            document.getElementById('editNombre').value = cliente.nombre;
            document.getElementById('editApellido').value = cliente.apellido;
            document.getElementById('editCorreo').value = cliente.correo;
            document.getElementById('editDepartamento').value = cliente.departamento;
            document.getElementById('editDni').value = cliente.dni;
            document.getElementById('editNumeroDeTelefono').value = cliente.numeroDeTelefono;
            document.getElementById('editPais').value = cliente.pais;
            document.getElementById('editUbicacionDeVivienda').value = cliente.ubicacionDeVivienda;
            document.getElementById('editEnlaceImagen').value = cliente.enlaceImagen;
            abrirModalEditar();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

function abrirModalEditar() {
    document.getElementById('modal-editar').style.display = 'block';
}

function cerrarModalEditar() {
    document.getElementById('modal-editar').style.display = 'none';
}

function guardarEdicion() {
    const clienteId = document.getElementById('editId').value;
    const nombre = document.getElementById('editNombre').value;
    const apellido = document.getElementById('editApellido').value;
    const correo = document.getElementById('editCorreo').value;
    const departamento = document.getElementById('editDepartamento').value;
    const dni = document.getElementById('editDni').value;
    const numeroDeTelefono = document.getElementById('editNumeroDeTelefono').value;
    const pais = document.getElementById('editPais').value;
    const ubicacionDeVivienda = document.getElementById('editUbicacionDeVivienda').value;
    const enlaceImagen = document.getElementById('editEnlaceImagen').value;

    const clienteEditado = { id: clienteId, nombre, apellido, correo, departamento, dni, numeroDeTelefono, pais, ubicacionDeVivienda, enlaceImagen };

    fetch(`http://localhost:8081/clientes/${clienteId}`, {
        method: 'PUT',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(clienteEditado),
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            cerrarModalEditar();
            obtenerClientes();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

function eliminarCliente(clienteId) {
    fetch(`http://localhost:8081/clientes/${clienteId}`, {
        method: 'DELETE',
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            obtenerClientes();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}