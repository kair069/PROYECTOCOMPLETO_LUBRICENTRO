let pedidos = [];

function obtenerPedidos() {
    fetch('http://localhost:8081/pedidos')
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(data => {
            pedidos = data;
            mostrarPedidos(pedidos);
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

function mostrarPedidos(pedidos) {
    const pedidosSection = document.getElementById('pedidos');
    pedidosSection.innerHTML = '';

    pedidos.forEach(pedido => {
        const pedidoElement = document.createElement('div');
        pedidoElement.classList.add('pedido');
        pedidoElement.innerHTML = `
            <h3>ID del Pedido: ${pedido.id}</h3>
            <p>ID del Cliente: ${pedido.cliente.id}</p>
            <p>Fecha del Pedido: ${pedido.fechaPedido}</p>
            <p>Estado del Pedido: ${pedido.estadoPedido}</p>
            <p>Dirección de Envío: ${pedido.direccionEnvio}</p>
            <p>Método de Pago: ${pedido.metodoPago}</p>
            <p>Total del Pedido: ${pedido.totalPedido}</p>
            <p>Número de Seguimiento: ${pedido.numeroSeguimiento}</p>
            <p>Comentarios: ${pedido.comentarios}</p>
            <p>Código de Descuento: ${pedido.codigoDescuento}</p>
            <p>Descuento Aplicado: ${pedido.descuentoAplicado}</p>
            <button onclick="editarPedido(${pedido.id})">Editar</button>
            <button onclick="eliminarPedido(${pedido.id})">Eliminar</button>
            <button class="button-ver-nombre" onclick="verNombreCliente(${pedido.cliente.id})">Ver Nombre del Cliente</button>
        `;
        pedidosSection.appendChild(pedidoElement);
    });
}
function enviarPedido() {
    // Lógica para enviar el pedido
    const clienteId = document.getElementById('clienteId').value;
    const fechaPedido = document.getElementById('fechaPedido').value;
    const estadoPedido = document.getElementById('estadoPedido').value;
    const direccionEnvio = document.getElementById('direccionEnvio').value;
    const metodoPago = document.getElementById('metodoPago').value;
    const totalPedido = parseFloat(document.getElementById('totalPedido').value);
    const numeroSeguimiento = document.getElementById('numeroSeguimiento').value;
    const comentarios = document.getElementById('comentarios').value;
    const codigoDescuento = document.getElementById('codigoDescuento').value;
    const descuentoAplicado = parseFloat(document.getElementById('descuentoAplicado').value);

    const pedido = {
        id: pedidos.length + 1,
        cliente: { id: clienteId },
        fechaPedido: fechaPedido,
        estadoPedido: estadoPedido,
        direccionEnvio: direccionEnvio,
        metodoPago: metodoPago,
        totalPedido: totalPedido,
        numeroSeguimiento: numeroSeguimiento,
        comentarios: comentarios,
        codigoDescuento: codigoDescuento,
        descuentoAplicado: descuentoAplicado,
        productosPedido: []
    };

    crearPedido(pedido);
}
function crearPedido(pedido) {
    fetch('http://localhost:8081/pedidos', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(pedido),
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            obtenerPedidos();
            cerrarModalAgregar();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}


function editarPedido(pedidoId) {
    fetch(`http://localhost:8081/pedidos/${pedidoId}`)
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(pedido => {
            // Asigna los valores actuales del pedido a los campos del formulario de edición
            document.getElementById('editId').value = pedido.id;
            document.getElementById('editClienteId').value = pedido.cliente.id;
            document.getElementById('editDireccionEnvio').value = pedido.direccionEnvio;
            document.getElementById('editMetodoPago').value = pedido.metodoPago;
            document.getElementById('editTotalPedido').value = pedido.totalPedido;
            document.getElementById('editNumeroSeguimiento').value = pedido.numeroSeguimiento;
            document.getElementById('editComentarios').value = pedido.comentarios;
            document.getElementById('editCodigoDescuento').value = pedido.codigoDescuento;
            document.getElementById('editDescuentoAplicado').value = pedido.descuentoAplicado;
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
    const pedidoId = document.getElementById('editId').value;
    const clienteId = document.getElementById('editClienteId').value;
    const direccionEnvio = document.getElementById('editDireccionEnvio').value;
    const metodoPago = document.getElementById('editMetodoPago').value;
    const totalPedido = parseFloat(document.getElementById('editTotalPedido').value);
    const numeroSeguimiento = document.getElementById('editNumeroSeguimiento').value;
    const comentarios = document.getElementById('editComentarios').value;
    const codigoDescuento = document.getElementById('editCodigoDescuento').value;
    const descuentoAplicado = parseFloat(document.getElementById('editDescuentoAplicado').value);

    const pedidoEditado = {
        id: pedidoId,
        cliente: { id: clienteId },
        direccionEnvio: direccionEnvio,
        metodoPago: metodoPago,
        totalPedido: totalPedido,
        numeroSeguimiento: numeroSeguimiento,
        comentarios: comentarios,
        codigoDescuento: codigoDescuento,
        descuentoAplicado: descuentoAplicado,
        productosPedido: []
    };

    fetch(`http://localhost:8081/pedidos/${pedidoId}`, {
        method: 'PUT',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(pedidoEditado),
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            cerrarModalEditar();
            obtenerPedidos();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

function eliminarPedido(pedidoId) {
    fetch(`http://localhost:8081/pedidos/${pedidoId}`, {
        method: 'DELETE',
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            // No intentamos analizar la respuesta JSON aquí, ya que DELETE puede no devolver datos JSON
            return response.text(); // Leemos el texto de la respuesta
        })
        .then(() => {
            obtenerPedidos();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}
