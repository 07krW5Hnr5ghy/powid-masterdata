package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

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
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableShipment, schema = Constants.schemaInventory)
public class Shipment {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_embarque")
    private Long id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "observaciones")
    private String observations;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "id_proveedor_producto")
    private Long supplierProductId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "id_compra")
    private Long purchaseId;

    @Column(name = "id_movimiento_inventario")
    private Long stockTransactionId;

    @Column(name = "id_warehouse")
    private Long warehouseId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "id_proveedor_producto", columnDefinition = "supplierProductId", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "id_compra", columnDefinition = "purchaseId", insertable = false, updatable = false)
    private Purchase purchase;

    @ManyToOne
    @JoinColumn(name = "id_movimiento_inventario", columnDefinition = "stockTransactionId", insertable = false, updatable = false)
    private StockTransaction stockTransaction;

    @ManyToOne
    @JoinColumn(name = "id_warehouse", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;
}
