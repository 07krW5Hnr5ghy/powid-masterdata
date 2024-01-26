package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOrderStock, schema = Constants.schemaInventory)
public class OrderStock {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_inventario_pedido")
    private Long id;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "id_item")
    private Long itemId;

    @Column(name = "id_proveedor_product")
    private Long supplierProductId;

    @Column(name = "id_pedido")
    private Long orderId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_item", columnDefinition = "itemId", insertable = false, updatable = false)
    private Item item;

    @ManyToOne
    @JoinColumn(name = "id_pedido", columnDefinition = "orderId", insertable = false, updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
