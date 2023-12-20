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
@Table(name = Constants.tableWarehouseStock, schema = Constants.schemaInventory)
public class WarehouseStock {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_almacen_inventario")
    private Long id;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "precio_compra_unitario")
    private Double unitPurchasePrice;

    @Column(name = "precio_compra_total")
    private Double totalPurchasePrice;

    @Column(name = "id_almacen")
    private Long warehouseId;

    @Column(name = "id_proveedor_producto")
    private Long supplierProductId;

    @Column(name = "id_embarque")
    private Long shipmentId;

    @ManyToOne
    @JoinColumn(name = "id_almacen", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "id_proveedor_producto", columnDefinition = "supplierProductId", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "id_embarque", columnDefinition = "shipmentId", insertable = false, updatable = false)
    private Shipment shipment;

}
