package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tablePayment, schema = Constants.schemaMaster)
public class Payment {
    @Id
    @GeneratedValue(generator = "sequence-payment")
    @GenericGenerator(
            name = "sequence-payment",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "pago_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_pago", unique = true)
    private Long id;

    @Column(name = "pago_total ")
    private double totalPayment;

    @Column(name = "descuento")
    private double discount;

    @Column(name = "mes", unique = true)
    private String month;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "id_canal", unique = true)
    private Long idChannel;

    @ManyToOne
    @JoinColumn(name = "id_canal", columnDefinition = "idChannel",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_canal"))
    private Channel channel;
}
