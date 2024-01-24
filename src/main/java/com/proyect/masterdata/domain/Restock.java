package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

@Entity
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableRestock,schema = Constants.schemaInventory)
public class Restock {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_reposicion")
    private Long id;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "id_producto")
    private Long productId;

    @Column(name = "id_pedido")
    private Long orderId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_producto", columnDefinition = "productId", insertable = false,updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "id_pedido", columnDefinition = "orderId", insertable = false,updatable = false)
    private Order order;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;
}
