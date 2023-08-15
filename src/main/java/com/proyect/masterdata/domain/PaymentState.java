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
@Table(name= Constants.tablePaymentState,schema=Constants.schemaMaster)
public class PaymentState {

    @Id
    @GeneratedValue(generator = "sequence-payment-state")
    @GenericGenerator(
            name = "sequence-state",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "estados_pago_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )

    @Column(name = "id_estado_pago", unique = true)
    private Long id;

    @Column(name = "nombre",length=50,unique = true)
    private String name;

    @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean status = true;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name="usuario")
    private String user;

}
