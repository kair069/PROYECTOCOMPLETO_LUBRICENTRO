document.addEventListener("DOMContentLoaded", function () {
    const apiUrl = "http://localhost:8081/ventas";
    const ventaForm = document.getElementById("ventaForm");
    const ventasTable = document.getElementById("ventasTable");
    const ventasBody = document.getElementById("ventasBody");

    // Función para cargar las ventas al cargar la página
    function cargarVentas() {
        fetch(apiUrl)
            .then((response) => response.json())
            .then((data) => mostrarVentas(data))
            .catch((error) => console.error("Error al cargar las ventas:", error));
    }

    // Función para mostrar las ventas en la tabla
    function mostrarVentas(ventas) {
        ventasBody.innerHTML = "";
        ventas.forEach((venta) => {
            const row = document.createElement("tr");
            // Añadir las demás columnas según tu modelo Venta
            row.innerHTML = `
                <td>${venta.id}</td>
                <td>${venta.marcaVehiculo}</td>
                <td>${venta.modeloVehiculo}</td>
                <td>${venta.anoVehiculo}</td>
                <td>${venta.tipoAceite}</td>
                <td>${venta.cantidadAceite}</td>
                <td>${venta.kilometrajeActual}</td>
                <td>${venta.cambioFiltroAceite}</td>
                <td>${venta.cambioOtrosFluidos}</td>
                <td>${venta.notasInspeccion}</td>
                <td>${venta.costoAceite}</td>
                <td>${venta.costoServicio}</td>
                <td>${venta.estadoVehiculo}</td>
                <td>${venta.recomendaciones}</td>
                <td>${venta.tecnicoEncargado}</td>
                <td>${venta.fechaVenta}</td>
                <td>
                <button onclick="editarVenta(${venta.id})">Editar</button>
                <button onclick="eliminarVenta(${venta.id})">Eliminar</button>
            </td>
            `;
            ventasBody.appendChild(row);
        });
    }

    // Manejar el envío del formulario para crear una nueva venta
    ventaForm.addEventListener("submit", function (event) {
        event.preventDefault();

        const formData = new FormData(ventaForm);
        const ventaData = {};
        formData.forEach((value, key) => {
            // Asegurarse de formatear la fecha correctamente
            if (key === 'fechaVenta') {
                const fechaVenta = new Date(value);
                ventaData[key] = fechaVenta.toISOString();
            } else {
                ventaData[key] = value;
            }
        });

        fetch(apiUrl, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(ventaData),
        })
            .then((response) => {
                if (!response.ok) {
                    throw new Error(`HTTP error! Status: ${response.status}`);
                }
                return response.json();
            })
            .then((data) => {
                cargarVentas(); // Recargar las ventas después de la creación exitosa
                ventaForm.reset(); // Resetear el formulario
            })
            .catch((error) => console.error("Error al crear la venta:", error));
    });

    // Cargar las ventas al cargar la página
    cargarVentas();
});

