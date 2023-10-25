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
@Table(name = Constants.tableChannel, schema = Constants.schemaMaster)
public class Channel {
    @Id
    @GeneratedValue(generator = "sequence-channel")
    @GenericGenerator(
            name = "sequence-channel",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "canal_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_canal", unique = true)
    private Long id;

    @Column(name = "nombre", unique = true)
    private String name;

    @Column(name = "meses")
    private int months;

    @Column(name = "estado")
    private boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "usuario", unique = true)
    private String user;

    @Column(name = "id_cliente", unique = true)
    private Long idClient;

    @Column(name = "id_membresia", unique = true)
    private Long idMembership;

    @Column(name = "id_tipo_pago")
    private Long idPaymentMethod;

    @Column(name = "id_conexion", unique = true)
    private Long idConnection;

    @ManyToOne
    @JoinColumn(name = "usuario", columnDefinition = "user",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_user"))
    private User datauser;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "idClient",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_cliente"))
    private Client client;

    @ManyToOne
    @JoinColumn(name = "id_membresia", columnDefinition = "idMembresia",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_membresia"))
    private Membership membership;

    @ManyToOne
    @JoinColumn(name = "id_tipo_pago", columnDefinition = "idPaymentType",insertable = false, updatable = false)
    private PaymentMethod paymentMethod;

    @ManyToOne
    @JoinColumn(name = "id_conexion", columnDefinition = "idConnection",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_conexion"))
    private Connection connection;
}
