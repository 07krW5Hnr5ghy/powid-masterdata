package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOrderPaymentReceipt,schema = Constants.schemaOrder)
public class OrderPaymentReceipt {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_comprobante_pago")
    private Long id;

    @Column(name = "id_pedido")
    private Long orderId;

    @Column(name = "url_recibo_pago")
    private String paymentReceiptUrl;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "usuario_token")
    private String tokenUser;

    @Column(name = "id_cliente")
    private Long clientId;

    @ManyToOne
    @JoinColumn(name = "id_pedido",columnDefinition = "orderId",insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "id_cliente",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
