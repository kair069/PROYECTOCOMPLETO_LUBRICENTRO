// proveedor.js

function obtenerTodosLosProveedores() {
    fetch('http://localhost:8081/proveedores')
        .then(response => response.json())
        .then(data => mostrarResultado(data))
        .catch(error => console.error('Error:', error));
}

function obtenerProveedorPorId() {
    const proveedorId = prompt('Ingrese el ID del proveedor:');
    if (proveedorId) {
        fetch(`http://localhost:8081/proveedores/${proveedorId}`)
            .then(response => response.json())
            .then(data => mostrarResultado([data]))
            .catch(error => console.error('Error:', error));
    }
}

function crearNuevoProveedor() {
    const nuevoProveedor = {
        nombre: prompt('Ingrese el nombre del nuevo proveedor:'),
        direccion: prompt('Ingrese la dirección del nuevo proveedor:'),
        correoElectronico: prompt('Ingrese el correo electrónico del nuevo proveedor:'),
        sitioWeb: prompt('Ingrese el sitio web del nuevo proveedor:'),
    };

    fetch('http://localhost:8081/proveedores', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(nuevoProveedor),
    })
        .then(response => response.json())
        .then(data => mostrarResultado([data]))
        .catch(error => console.error('Error:', error));
}

function actualizarProveedor() {
    const proveedorId = prompt('Ingrese el ID del proveedor que desea actualizar:');
    if (proveedorId) {
        const proveedorActualizado = {
            nombre: prompt('Ingrese el nuevo nombre del proveedor:'),
            direccion: prompt('Ingrese la nueva dirección del proveedor:'),
            correoElectronico: prompt('Ingrese el nuevo correo electrónico del proveedor:'),
            sitioWeb: prompt('Ingrese el nuevo sitio web del proveedor:'),
        };

        fetch(`http://localhost:8081/proveedores/${proveedorId}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(proveedorActualizado),
        })
            .then(response => response.json())
            .then(data => mostrarResultado([data]))
            .catch(error => console.error('Error:', error));
    }
}

function eliminarProveedor() {
    const proveedorId = prompt('Ingrese el ID del proveedor que desea eliminar:');
    if (proveedorId) {
        fetch(`http://localhost:8081/proveedores/${proveedorId}`, {
            method: 'DELETE',
        })
            .then(response => {
                if (response.ok) {
                    alert('Proveedor eliminado exitosamente');
                } else {
                    alert('Error al eliminar el proveedor. Verifica el ID e intenta nuevamente.');
                }
            })
            .catch(error => console.error('Error:', error));
    }
}

function buscarProveedoresPorNombre() {
    const nombreBusqueda = prompt('Ingrese el nombre del proveedor que desea buscar:');
    if (nombreBusqueda) {
        const nombreCodificado = encodeURIComponent(nombreBusqueda);
        fetch(`http://localhost:8081/proveedores/buscarPorNombre/${nombreCodificado}`)
            .then(response => {
                if (response.ok) {
                    return response.json();
                } else {
                    throw new Error('Error en la solicitud de búsqueda');
                }
            })
            .then(data => mostrarResultado(data))
            .catch(error => console.error('Error:', error));
    }
}

function obtenerProveedorPorCorreoElectronico() {
    const correoElectronico = prompt('Ingrese el correo electrónico del proveedor que desea obtener:');
    if (correoElectronico) {
        fetch(`http://localhost:8081/proveedores/buscarPorCorreo/${encodeURIComponent(correoElectronico)}`)
            .then(response => {
                if (response.ok) {
                    return response.json();
                } else {
                    throw new Error('Error en la solicitud para obtener por correo electrónico');
                }
            })
            .then(data => mostrarResultado([data]))
            .catch(error => console.error('Error:', error));
    }
}

// proveedor.js

// proveedor.js

function obtenerProveedorPorSitioWeb() {
    const sitioWeb = prompt('Ingrese el sitio web del proveedor que desea obtener:');
    if (sitioWeb) {
        fetch(`http://localhost:8081/proveedores/buscarPorSitioWeb/${encodeURIComponent(sitioWeb)}`)
            .then(response => {
                if (response.ok) {
                    return response.json();
                } else {
                    throw new Error('Error en la solicitud para obtener por sitio web');
                }
            })
            .then(data => mostrarResultado([data]))
            .catch(error => console.error('Error:', error));
    }
}



function obtenerProveedoresPorRangoFechas() {
    const fechaInicio = prompt('Ingrese la fecha de inicio del rango (YYYY-MM-DD):');
    const fechaFin = prompt('Ingrese la fecha de fin del rango (YYYY-MM-DD):');

    if (fechaInicio && fechaFin) {
        fetch(`http://localhost:8081/proveedores/porRangoFechas/${encodeURIComponent(fechaInicio)}/${encodeURIComponent(fechaFin)}`)
            .then(response => {
                if (response.ok) {
                    return response.json();
                } else {
                    throw new Error('Error en la solicitud para obtener por rango de fechas');
                }
            })
            .then(data => mostrarResultado(data))
            .catch(error => console.error('Error:', error));
    }
}

function contarTotalProveedores() {
    fetch('http://localhost:8081/proveedores/contarTotal')
        .then(response => {
            if (response.ok) {
                return response.json();
            } else {
                throw new Error('Error en la solicitud para contar el total de proveedores');
            }
        })
        .then(data => {
            alert(`Total de Proveedores: ${data}`);
        })
        .catch(error => console.error('Error:', error));
}

function mostrarResultado(proveedores) {
    const resultadoDiv = document.getElementById('resultado');
    resultadoDiv.innerHTML = ''; // Limpiar el contenido anterior

    if (Array.isArray(proveedores)) {
        proveedores.forEach(proveedor => {
            const proveedorDiv = document.createElement('div');
            proveedorDiv.innerHTML = `
                <p>ID: ${proveedor.id}</p>
                <p>Nombre: ${proveedor.nombre}</p>
                <p>Dirección: ${proveedor.direccion}</p>
                <p>Correo Electrónico: ${proveedor.correoElectronico}</p>
                <p>Sitio Web: ${proveedor.sitioWeb}</p>
                <hr>
            `;
            resultadoDiv.appendChild(proveedorDiv);
        });
    } else {
        // Manejar el caso en el que no se encontraron proveedores
        resultadoDiv.innerHTML = 'No se encontraron proveedores con el nombre proporcionado.';
    }
}

// proveedor.js

function listarTodosLosCampos() {
    fetch('http://localhost:8080/miaplicacion/proveedores')
        .then(response => {
            if (response.ok) {
                return response.json();
            } else {
                throw new Error('Error en la solicitud para obtener todos los proveedores');
            }
        })
        .then(data => {
            // Limpia el contenido existente
            document.getElementById('resultado').innerHTML = '';

            // Muestra la información de cada proveedor
            data.forEach(proveedor => {
                const proveedorInfo = document.createElement('div');
                proveedorInfo.innerHTML = `
                    <p>ID: ${proveedor.id}</p>
                    <p>Nombre: ${proveedor.nombre}</p>
                    <p>Dirección: ${proveedor.direccion}</p>
                    <p>Teléfono: ${proveedor.telefono}</p>
                    <p>Correo Electrónico: ${proveedor.correoElectronico}</p>
                    <p>Sitio Web: ${proveedor.sitioWeb}</p>
                    <p>Contacto Principal: ${proveedor.contactoPrincipal}</p>
                    <p>Fecha de Caducidad: ${proveedor.fechaCaducidad}</p>
                    <p>Fecha de Inicio: ${proveedor.fechaInicio}</p>
                    <hr>
                `;
                document.getElementById('resultado').appendChild(proveedorInfo);
            });
        })
        .catch(error => console.error('Error:', error));
}

// Llama a la función al cargar la página
document.addEventListener('DOMContentLoaded', listarTodosLosCampos);

// Agrega un evento para volver a llamar a la función al actualizar la página
window.addEventListener('beforeunload', listarTodosLosCampos);
