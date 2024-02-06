package com.proyect.masterdata.domain;

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
@Table(name = Constants.tableSale, schema = Constants.schemaOrder)
public class Sale {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_venta")
    private Long id;

    @Column(name = "vendedor")
    private String seller;

    @Column(name = "observaciones")
    private String observations;

    @Column(name = "direccion_entrega")
    private String deliveryAddress;

    @Column(name = "monto_venta")
    private Double saleAmount;

    @Column(name = "monto_envio")
    private Double deliveryAmount;

    @Column(name = "pago_adelantado")
    private Double advancePayment;

    @Column(name = "pago_pendiente")
    private Double duePayment;

    @Column(name = "id_canal_venta")
    private Long saleChannelId;

    @Column(name = "id_estado_pago")
    private Long paymentStateId;

    @Column(name = "id_metodo_pago")
    private Long paymentMethodId;

    @Column(name = "id_tipo_gestion")
    private Long managementTypeId;

    @Column(name = "id_orden")
    private Long orderId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_orden", columnDefinition = "orderId", insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "id_canal_venta", columnDefinition = "saleChannelId", insertable = false, updatable = false)
    private SaleChannel saleChannel;

    @ManyToOne
    @JoinColumn(name = "id_estado_pago", columnDefinition = "paymentStateId", insertable = false, updatable = false)
    private PaymentState paymentState;

    @ManyToOne
    @JoinColumn(name = "id_metodo_pago", columnDefinition = "paymentMethodId", insertable = false, updatable = false)
    private PaymentMethod paymentMethod;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "id_tipo_gestion", columnDefinition = "managementTypeId", insertable = false, updatable = false)
    private ManagementType managementType;

}
