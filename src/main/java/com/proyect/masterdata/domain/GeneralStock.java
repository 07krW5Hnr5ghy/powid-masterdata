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
@Table(name = Constants.tableGeneralStock, schema = Constants.schemaInventory)
public class GeneralStock {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_inventario_general")
    private Long id;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "id_proveedor_producto")
    private Long supplierProductId;

    @Column(name = "id_cliente")
    private Long clientId;

    @ManyToOne
    @JoinColumn(name = "id_proveedor_producto", columnDefinition = "supplierProductId", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;
}
