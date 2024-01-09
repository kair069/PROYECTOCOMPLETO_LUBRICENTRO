document.addEventListener('DOMContentLoaded', () => {
    obtenerProductos();

    // Agrega un listener para el evento submit del formulario de creación
    document.getElementById('formulario-producto').addEventListener('submit', function (event) {
        event.preventDefault();
        const nombre = document.getElementById('nombre').value;
        const precio = document.getElementById('precio').value;
        const stock = document.getElementById('stock').value;

        crearProducto({ nombre, precio, stock });
    });

    // Agrega un listener para el botón de guardar cambios en el formulario de edición
    document.getElementById('formulario-editar').addEventListener('submit', function (event) {
        event.preventDefault();
        guardarEdicion();
    });
});

// Función para obtener todos los productos
function obtenerProductos() {
    fetch('http://localhost:8081/productos')
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(data => {
            mostrarProductos(data);
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

// Función para mostrar la lista de productos en la interfaz

// function mostrarProductos(productos) {
//     const productosSection = document.getElementById('productos');
//     productosSection.innerHTML = '';

//     productos.forEach(producto => {
//         const productoElement = document.createElement('div');
//         productoElement.classList.add('producto');
//         productoElement.innerHTML = `
//             <h3>${producto.nombre}</h3>
//             <p>Precio: ${producto.precio}</p>
//             <p>Stock: ${producto.stock}</p>
//             <button onclick="editarProducto(${producto.id})">Editar</button>
//             <button onclick="eliminarProducto(${producto.id})">Eliminar</button>
//         `;
//         productosSection.appendChild(productoElement);
//     });
// }

// function mostrarProductos(productos) {
//     const productosSection = document.getElementById('productos');
//     productosSection.innerHTML = ''; // Limpiar la lista antes de mostrar los productos

//     const filtro = document.getElementById('buscar').value.toLowerCase();

//     productos.forEach(producto => {
//         // Filtra los productos que coincidan con el término de búsqueda
//         if (producto.nombre.toLowerCase().includes(filtro)) {
//             const productoElement = document.createElement('div');
//             productoElement.classList.add('producto');
//             productoElement.innerHTML = `
//                 <h3>${producto.nombre}</h3>
//                 <p>Precio: ${producto.precio}</p>
//                 <p>Stock: ${producto.stock}</p>
//                 <button onclick="editarProducto(${producto.id})">Editar</button>
//                 <button onclick="eliminarProducto(${producto.id})">Eliminar</button>
//             `;
//             productosSection.appendChild(productoElement);
//         }
//     });
// }
function mostrarProductos(productos) {
    const productosSection = document.getElementById('productos');
    productosSection.innerHTML = ''; // Limpiar la lista antes de mostrar los productos

    const filtro = document.getElementById('buscar').value.toLowerCase();
    



    productos.forEach(producto => {
        // Filtra los productos que coincidan con el término de búsqueda
        if (producto.nombre.toLowerCase().includes(filtro)) {
            const productoElement = document.createElement('div');
            productoElement.classList.add('producto');
            const imagenLink = obtenerLinkImagen(producto.nombre);
            productoElement.innerHTML = `
                <div class="info-producto">
                    <h3>${producto.nombre}</h3>
                    <p>Precio: ${producto.precio}</p>
                    <p>Stock: ${producto.stock}</p>
                </div>
                <div class="imagen-producto">
                    <img src="${imagenLink}" alt="${producto.nombre}">
                </div>
                <div class="acciones-producto">
                    <button onclick="editarProducto(${producto.id})">Editar</button>
                    <button onclick="eliminarProducto(${producto.id})">Eliminar</button>
                </div>
            `;
            productosSection.appendChild(productoElement);
        }
    });
}

function obtenerLinkImagen(nombreProducto) {
    const enlacesImagenes = {
        'ACEITE SHEEL': 'https://lubrimas.pe/tienda/wp-content/uploads/sites/3/2022/02/lubrimasperu-aceite-shell-helix-hx5-20w50-mineral-gasolina-gas-mineral-galon-4lt-losolivos-lima-peru.webp',
        'ACEITE SHELL': 'https://lubrimas.pe/tienda/wp-content/uploads/sites/3/2022/02/lubrimasperu-aceite-shell-helix-hx5-20w50-mineral-gasolina-gas-mineral-galon-4lt-losolivos-lima-peru.webp',
        'ACEITE REPSOL': 'https://viperautomotora.com/wp-content/uploads/2021/06/Repsol-20w50-1L.jpg',
        'ACEITE SHEVRON 10w-30': 'https://lubrimas.pe/tienda/wp-content/uploads/sites/3/2022/11/lubrimaspe-lubrimasperu-aceite-mineral-chevron-supreme-20w50-api-sp-galon-1lt-losolivos-lima-peru.webp',
        'FILTRO DE AIRE': 'https://dojiw2m9tvv09.cloudfront.net/74294/product/9-hp1201.png',
        'FILTRO DE ACEITE': 'https://falabella.scene7.com/is/image/FalabellaPE/gsc_118916059_2200519_1?wid=800&hei=800&qlt=70',
        'ACEITE CASTROL':'https://dch.com.pe/wp-content/uploads/2019/02/GTX-10w30.png',
        'Mobil Delvac 15w-40':'https://promart.vteximg.com.br/arquivos/ids/6551450-1000-1000/132435.jpg?v=638001767838500000',
        'Castrol EDGE':'https://lubrimas.pe/tienda/wp-content/uploads/sites/3/2022/07/lubrimas_pe-lubrimasperu-aceite-castrol-edge-eurocar-5w30-k-fullsintetico-galon-4_73lt-losolivos-lima-peru.webp',
        'Shell Rotella T4':'https://rotella.shell.com/en_us/products/triple-protection-motor-oil/rotella-triple-protection/_jcr_content/par/productDetails/image.img.960.jpeg/1525517931349/shell-oil4-28-16-484-t4-palette.jpeg?imwidth=960',
        'Valvoline Daily Protection':'https://m.media-amazon.com/images/I/61TFzOCKyAL._AC_SL1500_.jpg',
        'Pennzoil Platinum':'https://www.ubuy.pe/productimg/?image=aHR0cHM6Ly9pbWFnZXMtY2RuLnVidXkuY29tLnNhLzYzM2ZlZGM2ODRjMjE5NDU3NDE5MjBhMi1tb3Bhci0xLXF1YXJ0LW9mLXBlbm56b2lsLXBsYXRpbnVtLWZ1bGwuanBn.jpg',
        'Quaker State Ultimate Durability':'https://m.media-amazon.com/images/I/81VWkAI8krL.jpg',
        'Royal Purple HMX':'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSl6NiYv-pYyCxyEwyuNW9_YX0aIl0vxsl65PztMFncO1Og-FKBMfJQJlb2GKKwR0r_UNU&usqp=CAU',
        'Motul 8100 X-cess':'https://www.google.com/url?sa=i&url=https%3A%2F%2Fnew.motul.com%2Fes-ES%2Fproducts%2F17600&psig=AOvVaw0j1Wk2iD4ggk-kvjNHCCR5&ust=1703482661551000&source=images&cd=vfe&opi=89978449&ved=0CBEQjRxqFwoTCPiAscitp4MDFQAAAAAdAAAAABAI',
        'Total Quartz 9000':'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcS6kIarlIruUeZivvJ38K7AraP9V99HmaoGJENUhbGHwR27wYFurbiGq-jS4PQXXK0jszE&usqp=CAU',
        'Liqui Moly Leichtlauf High Tech':'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTjwglCf9aUqCTNLulTBsv3i6OYqZvTunF5XK_ExzHWZEi5eoK7ru2bHRSpNd8IZ080yec&usqp=CAU'
        // Agrega más enlaces según sea necesario
    };

    return enlacesImagenes[nombreProducto] || 'URL_POR_DEFECTO_SI_NO_EXISTE';
}
function filtrarProductos() {
    obtenerProductos(); // Vuelve a cargar la lista de productos después de aplicar el filtro
}

// Función para crear un nuevo producto
function crearProducto(producto) {
    fetch('http://localhost:8081/productos', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(producto),
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            obtenerProductos();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

// Función para editar un producto
function editarProducto(productoId) {
    fetch(`http://localhost:8081/productos/${productoId}`)
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(producto => {
            document.getElementById('editId').value = producto.id;
            document.getElementById('editNombre').value = producto.nombre;
            document.getElementById('editPrecio').value = producto.precio;
            document.getElementById('editStock').value = producto.stock;
            abrirModalEditar();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}

// Función para abrir el modal de edición
function abrirModalEditar() {
    const modalEditar = document.getElementById('modal-editar');
    if (modalEditar) {
        modalEditar.style.display = 'block';
    }
}

// Función para cerrar el modal de edición
function cerrarModalEditar() {
    const modalEditar = document.getElementById('modal-editar');
    if (modalEditar) {
        modalEditar.style.display = 'none';
    }
}

// Función para guardar los cambios de edición
function guardarEdicion() {
    const productoId = document.getElementById('editId').value;
    const nombre = document.getElementById('editNombre').value;
    const precio = document.getElementById('editPrecio').value;
    const stock = document.getElementById('editStock').value;

    const productoEditado = { id: productoId, nombre, precio, stock };

    fetch(`http://localhost:8081/productos/${productoId}`, {
        method: 'PUT',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(productoEditado),
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            cerrarModalEditar();
            obtenerProductos();
        })
        .catch(error => {
            console.error('Error al guardar la edición:', error.message);
        });
}

// Función para eliminar un producto
function eliminarProducto(productoId) {
    fetch(`http://localhost:8081/productos/${productoId}`, {
        method: 'DELETE',
    })
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json();
        })
        .then(() => {
            obtenerProductos();
        })
        .catch(error => {
            console.error('Error en la solicitud:', error);
        });
}
