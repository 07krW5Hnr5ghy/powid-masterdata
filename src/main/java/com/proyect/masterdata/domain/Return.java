package com.proyect.masterdata.domain;

import java.sql.Date;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Table(name = Constants.tableReturn, schema = Constants.schemaInventory)
public class Return {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_devolucion")
    private Long id;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "fecha_registro")
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    private Date updateDate;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "id_proveedor_producto")
    private Long supplierProductId;

    @Column(name = "id_compra")
    private Long purchaseId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "usuarioToken")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_proveedor_producto", columnDefinition = "supplierProduct", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "id_compra", columnDefinition = "purchaseId", insertable = false, updatable = false)
    private Purchase purchase;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
