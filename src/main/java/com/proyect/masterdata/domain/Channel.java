package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableChannel, schema = Constants.schemaMaster)
public class Channel {
    @Id
    @GeneratedValue(generator = "sequence-generator")
    @GenericGenerator(
            name = "sequence-generator",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "canal_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_canal", unique = true)
    private Long id;

    @Column(name = "usuario", unique = true)
    private String user;

    @Column(name = "id_cliente", unique = true)
    private Long idClient;

    @Column(name = "id_menbresia", unique = true)
    private Long idMenbresia;

    @Column(name = "id_pago", unique = true)
    private Long idPayment;

    @Column(name = "id_descuento", unique = true)
    private Long idDiscount;

    @Column(name = "id_tipo_pago", unique = true)
    private Long idPaymentType;

    @ManyToOne
    @JoinColumn(name = "usuario", columnDefinition = "user",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_sesion"))
    private Login login;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "idClient",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_cliente"))
    private Client client;

    @ManyToOne
    @JoinColumn(name = "id_menbresia", columnDefinition = "idMenbresia",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_menbresia"))
    private Menbresia menbresia;

    @ManyToOne
    @JoinColumn(name = "id_pago", columnDefinition = "idPayment",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_pago"))
    private Payment payment;

    @ManyToOne
    @JoinColumn(name = "id_descuento", columnDefinition = "idDiscount",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_descuento"))
    private Discount discount;

    @ManyToOne
    @JoinColumn(name = "id_tipo_pago", columnDefinition = "idPaymentType",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_tipo_pago"))
    private PaymentType paymentType;
}
