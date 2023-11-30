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
@Table(name = Constants.tablePayment, schema = Constants.schemaManagement)
public class Payment {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_pago", nullable = false)
        private Long id;

        @Column(name = "meses", nullable = false)
        private Integer months;

        @Column(name = "monto_neto", nullable = false)
        private Double netAmount;

        @Column(name = "monto_bruto", nullable = false)
        private Double grossAmount;

        @Column(name = "url_factura")
        private String urlInvoice;

        @Column(name = "fecha_registro", nullable = false)
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "id_subscripcion", nullable = false)
        private Long subscriptionId;

        @Column(name = "id_estado_pago", nullable = false)
        private Long paymentStateId;

        @Column(name = "id_metodo_pago", nullable = false)
        private Long paymentMethodId;

        @JoinColumn(name = "id_subscripcion", columnDefinition = "subscriptionId", insertable = false, updatable = false)
        private Subscription subscription;

        @JoinColumn(name = "id_estado_pago", columnDefinition = "paymentStateId", insertable = false, updatable = false)
        private PaymentState paymentState;

        @JoinColumn(name = "id_metodo_pago", columnDefinition = "paymentMethodId", insertable = false, updatable = false)
        private PaymentMethod paymentMethod;

}
