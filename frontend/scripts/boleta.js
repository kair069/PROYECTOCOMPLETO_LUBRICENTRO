const apiUrl = 'http://localhost:8081/api/boletas';

// Función para obtener y mostrar las boletas
async function getBoletas() {
    try {
        const response = await fetch(apiUrl);
        const data = await response.json();

        const boletasList = document.getElementById('boletas-list');
        boletasList.innerHTML = '';

        data.forEach(boleta => {
            const listItem = document.createElement('li');

            // Verifica si 'detalles' y 'detalles[0]' están definidos antes de acceder a 'cantidad' y 'producto'
            const cantidad = boleta.detalles && boleta.detalles[0] ? boleta.detalles[0].cantidad : 'N/A';
            const precioUnitario = boleta.detalles && boleta.detalles[0] ? boleta.detalles[0].precioUnitario : 'N/A';
            const productoId = boleta.detalles && boleta.detalles[0] && boleta.detalles[0].producto ? boleta.detalles[0].producto.id : 'N/A';
            const productoNombre = boleta.detalles && boleta.detalles[0] && boleta.detalles[0].producto ? boleta.detalles[0].producto.nombre : 'N/A';
            const productoPrecio = boleta.detalles && boleta.detalles[0] && boleta.detalles[0].producto ? boleta.detalles[0].producto.precio : 'N/A';

            listItem.innerHTML = `
                <strong>ID:</strong> ${boleta.id}<br>
                <strong>Fecha:</strong> ${boleta.fecha}<br>
                <strong>Cliente:</strong><br>
                - Nombre: ${boleta.cliente ? boleta.cliente.nombre : 'N/A'}<br>
                - Apellido: ${boleta.cliente ? boleta.cliente.apellido : 'N/A'}<br>
                - Correo: ${boleta.cliente ? boleta.cliente.correo : 'N/A'}<br>
                - DNI: ${boleta.cliente ? boleta.cliente.dni : 'N/A'}<br>
                - Teléfono: ${boleta.cliente ? boleta.cliente.numeroDeTelefono : 'N/A'}<br>
                - País: ${boleta.cliente ? boleta.cliente.pais : 'N/A'}<br>
                - Ubicación de Vivienda: ${boleta.cliente ? boleta.cliente.ubicacionDeVivienda : 'N/A'}<br>
                - Enlace Imagen: <img src="${boleta.cliente ? boleta.cliente.enlaceImagen : ''}" alt="imagen cliente" width="100"><br>
                <strong>Detalles:</strong><br>
                - Cantidad: ${cantidad}<br>
                - Precio Unitario: ${precioUnitario}<br>
                - Producto:<br>
                  - ID: ${productoId}<br>
                  - Nombre: ${productoNombre}<br>
                  - Precio: ${productoPrecio}<br>
                <button onclick="editarBoleta(${boleta.id})">Editar</button>
                <button onclick="eliminarBoleta(${boleta.id})">Eliminar</button>
                <button onclick="showBoletaModal()">Ver Boleta</button>
                <button onclick="printBoleta()">Imprimir Boleta</button>
                
            `;
            boletasList.appendChild(listItem);
        });
    } catch (error) {
        console.error('Error al obtener boletas:', error.message);
    }
}

// Función para mostrar el formulario de creación
function showCreateForm() {
    document.getElementById('create-form').style.display = 'block';
}

// Función para enviar el formulario de creación
async function submitCreateForm() {
    const fecha = document.getElementById('fecha').value;
    const clienteId = document.getElementById('cliente-id').value;

    const nuevaBoleta = {
        fecha: fecha,
        cliente: {
            id: clienteId
        }
    };

    try {
        const response = await fetch(apiUrl, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(nuevaBoleta)
        });

        if (response.ok) {
            console.log('Boleta creada exitosamente');
            // Limpia el formulario
            document.getElementById('fecha').value = '';
            document.getElementById('cliente-id').value = '';

            // Oculta el formulario
            document.getElementById('create-form').style.display = 'none';

            // Actualiza la lista de boletas
            getBoletas();
        } else {
            console.error('Error al crear la boleta:', response.statusText);
        }
    } catch (error) {
        console.error('Error al crear la boleta:', error.message);
    }
}

// Función para editar una boleta
async function editarBoleta(id) {
    const nuevaFecha = prompt('Ingrese la nueva fecha:');
    const nuevoClienteId = prompt('Ingrese el nuevo ID del cliente:');

    try {
        await fetch(`${apiUrl}/${id}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                fecha: nuevaFecha,
                cliente: {
                    id: nuevoClienteId,
                },
            }),
        });

        alert('Boleta editada exitosamente');
        getBoletas(); // Actualiza la lista después de editar
    } catch (error) {
        console.error('Error al editar boleta:', error.message);
    }
}

// Función para eliminar una boleta
async function eliminarBoleta(id) {
    const confirmacion = confirm('¿Estás seguro de que quieres eliminar esta boleta?');

    if (confirmacion) {
        try {
            await fetch(`${apiUrl}/${id}`, {
                method: 'DELETE',
            });

            alert('Boleta eliminada exitosamente');
            getBoletas(); // Actualiza la lista después de eliminar
        } catch (error) {
            console.error('Error al eliminar boleta:', error.message);
        }
    }
}


// Llama a la función getBoletas al cargar la página
document.addEventListener('DOMContentLoaded', getBoletas);


function showBoletaModal() {
    const modal = document.getElementById('boleta-modal');
    modal.style.display = 'block';

    // Obtener el contenido de la última boleta de la lista
    const ultimaBoleta = document.getElementById('boletas-list').lastChild;
    const modalContent = document.getElementById('modal-content');
    modalContent.innerHTML = ultimaBoleta.innerHTML;

    // Agregar el botón de cierre al modal
    modalContent.innerHTML += `
        <button onclick="closeBoletaModal()">Cerrar</button>
    `;
}

// Función para cerrar el modal
function closeBoletaModal() {
    const modal = document.getElementById('boleta-modal');
    modal.style.display = 'none';
}

// Función para imprimir la boleta
function printBoleta() {
    const contenidoBoleta = document.getElementById('modal-content').innerHTML;

    // Abrir una nueva ventana
    const ventanaImpresion = window.open('', '_blank');

    // Escribir el contenido de la boleta en la nueva ventana
    ventanaImpresion.document.write('<html><head><title>Boleta</title></head><body>');
    ventanaImpresion.document.write(contenidoBoleta);
    ventanaImpresion.document.write('</body></html>');
    ventanaImpresion.document.close();

    // Esperar a que se cargue el contenido antes de intentar imprimir
    ventanaImpresion.onload = function() {
        ventanaImpresion.print();
        ventanaImpresion.onafterprint = function() {
            // Cerrar la ventana después de imprimir
            ventanaImpresion.close();
        };
    };
}