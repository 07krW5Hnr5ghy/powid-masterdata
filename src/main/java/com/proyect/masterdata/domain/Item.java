package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableItem, schema = Constants.schemaOrder)
public class Item {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_item")
    private Long id;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "descuento")
    private Double discount;

    @Column(name = "id_producto")
    private Long productId;

    @Column(name = "id_orden")
    private Long orderId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_producto", columnDefinition = "productId", insertable = false,updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "id_orden", columnDefinition = "orderId", insertable = false, updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
