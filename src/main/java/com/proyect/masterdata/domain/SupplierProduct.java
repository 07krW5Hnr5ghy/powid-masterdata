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
@Table(name = Constants.tableSupplierProduct, schema = Constants.schemaInventory)
public class SupplierProduct {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_proveedor_producto")
    private Long id;

    @Column(name = "precio_compra")
    private Double purchasePrice;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "id_proveedor")
    private Long supplierId;

    @Column(name = "id_producto")
    private Long productId;

    @ManyToOne
    @JoinColumn(name = "id_proveedor", columnDefinition = "supplierId", insertable = false, updatable = false)
    private Supplier supplier;

    @ManyToOne
    @JoinColumn(name = "id_producto", columnDefinition = "productId", insertable = false, updatable = false)
    private Product product;
}
