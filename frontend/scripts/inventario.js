document.addEventListener("DOMContentLoaded", function () {
    // Ruta del API
    const apiUrl = "http://localhost:8081/inventarios";
    let detalleInventarioActual; // Variable para almacenar el inventario actual

    // Obtener la lista de inventarios desde el API
    fetch(apiUrl)
        .then(response => response.json())
        .then(data => mostrarInventarios(data))
        .catch(error => console.error("Error al obtener datos del API:", error));

    // Función para mostrar la lista de inventarios en la página
    function mostrarInventarios(inventarios) {
        const listaInventarios = document.getElementById("inventario-lista");

        inventarios.forEach(inventario => {
            const listItem = document.createElement("li");
            listItem.className = "list-group-item";
            listItem.innerHTML = `
                <strong>ID:</strong> ${inventario.id}<br>
                <strong>Producto:</strong> ${inventario.producto.nombre}<br>
                <strong>Cantidad:</strong> ${inventario.cantidad}<br>
                <strong>Fecha de Ingreso:</strong> ${new Date(inventario.fechaIngreso).toLocaleDateString()}<br>
                <strong>Fecha de Caducidad:</strong> ${new Date(inventario.fechaCaducidad).toLocaleDateString()}<br>
                <strong>Kilometraje último cambio:</strong> ${inventario.kilometrajeUltimoCambio}<br>
                <strong>Tipo de Aceite:</strong> ${inventario.tipoAceite}<br>
                <strong>Marca de Aceite:</strong> ${inventario.marcaAceite}<br>
                <strong>Proveedor:</strong> ${inventario.proveedor.nombre}<br>
                
                <button type="button" class="btn btn-danger eliminar-btn" data-id="${inventario.id}">Eliminar</button>
                <button type="button" class="btn btn-info detalles-btn" data-id="${inventario.id}">Ver Detalles</button>
            `;
            listaInventarios.appendChild(listItem);
        });

        // Agregar listener a los botones de detalles
        const botonesDetalles = document.querySelectorAll('.detalles-btn');
        botonesDetalles.forEach(boton => {
            boton.addEventListener('click', function() {
                const idInventario = this.getAttribute('data-id');
                cargarDetallesInventario(idInventario);
            });
        });
        
        // Agregar listener a los botones de eliminar
        const botonesEliminar = document.querySelectorAll('.eliminar-btn');
        botonesEliminar.forEach(boton => {
            boton.addEventListener('click', function() {
                const idInventario = this.getAttribute('data-id');
                eliminarInventario(idInventario);
            });
        });
    }

    // Función para cargar los detalles de un inventario
    function cargarDetallesInventario(id) {
        fetch(`${apiUrl}/${id}`)
            .then(response => response.json())
            .then(data => {
                detalleInventarioActual = data;
                mostrarDetallesInventario(detalleInventarioActual);
            })
            .catch(error => console.error("Error al obtener detalles del inventario:", error));
    }

    // Función para mostrar los detalles del inventario en un modal
    function mostrarDetallesInventario(inventario) {
        const modal = document.getElementById('detallesInventarioModal');
        const modalBody = modal.querySelector('.modal-body');

        // Construir el contenido del modal
        modalBody.innerHTML = `
            <p><strong>ID:</strong> ${inventario.id}</p>
            <p><strong>Producto:</strong> ${inventario.producto.nombre}</p>
            <p><strong>Cantidad:</strong> ${inventario.cantidad}</p>
            <p><strong>Fecha de Ingreso:</strong> ${new Date(inventario.fechaIngreso).toLocaleDateString()}</p>
            <p><strong>Fecha de Caducidad:</strong> ${new Date(inventario.fechaCaducidad).toLocaleDateString()}</p>
            <p><strong>Kilometraje último cambio:</strong> ${inventario.kilometrajeUltimoCambio}</p>
            <p><strong>Tipo de Aceite:</strong> ${inventario.tipoAceite}</p>
            <p><strong>Marca de Aceite:</strong> ${inventario.marcaAceite}</p>
            <p><strong>Proveedor:</strong> ${inventario.proveedor.nombre}</p>
        `;

        // Mostrar el modal
        $('#detallesInventarioModal').modal('show');
    }

    // Función para realizar el POST con los datos del formulario
    function realizarPost(nuevoInventario) {
        fetch(apiUrl, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(nuevoInventario),
        })
        .then(response => response.json())
        .then(data => console.log('POST exitoso:', data))
        .catch(error => console.error("Error al realizar el POST:", error));
    }

    // Función para realizar la eliminación de un inventario
    function eliminarInventario(id) {
        fetch(`${apiUrl}/${id}`, {
            method: 'DELETE',
        })
        .then(response => {
            if (!response.ok) {
                throw new Error(`Error al realizar el DELETE: ${response.status} ${response.statusText}`);
            }
            console.log('DELETE exitoso');
        })
        .catch(error => console.error(error));
    }

    // Agrega el listener al botón después de que se cargue el DOM
    document.getElementById('enviarBtn').addEventListener('click', enviarFormulario);

    // Función para obtener los valores del formulario y realizar el POST
    function enviarFormulario() {
        const nuevoInventario = {
            id: 2, // Puedes ajustar el ID de inventario según tus necesidades
            producto: {
                id: parseInt(document.getElementById('productoId').value)
            },
            cantidad: parseInt(document.getElementById('cantidad').value),
            fechaIngreso: document.getElementById('fechaIngreso').value,
            fechaCaducidad: document.getElementById('fechaCaducidad').value,
            kilometrajeUltimoCambio: parseInt(document.getElementById('kilometraje').value),
            tipoAceite: document.getElementById('tipoAceite').value,
            marcaAceite: document.getElementById('marcaAceite').value,
           
            proveedor: {
                id: parseInt(document.getElementById('proveedorId').value)
            }
        };

        realizarPost(nuevoInventario);
    }
});
