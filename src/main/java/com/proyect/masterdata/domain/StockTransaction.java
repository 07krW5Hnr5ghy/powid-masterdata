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
@Table(name = Constants.tableStockTransaction, schema = Constants.schemaInventory)
public class StockTransaction {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_movimiento_inventario")
    private Long id;

    @Column(name = "cantidad")
    private Integer quantity;

    @Column(name = "fecha_regisstro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "id_tipo_movimiento_inventario")
    private Long stockTransactionTypeId;

    @Column(name = "id_proveedor_producto")
    private Long supplierProductId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "id_tipo_movimiento_inventario", columnDefinition = "stockTransactionTypeId", insertable = false, updatable = false)
    private StockTransactionType stockTransactionType;

    @ManyToOne
    @JoinColumn(name = "id_proveedor_producto", columnDefinition = "supplierProductId", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;
}
