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
@Table(name = Constants.tableClient, schema = Constants.schemaMaster)
public class Client {
    @Id
    @GeneratedValue(generator = "sequence-generator")
    @GenericGenerator(
            name = "sequence-generator",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "cliente_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_cliente", unique = true)
    private Long idClient;

    @Column(name = "nombre")
    private String name;

    @Column(name = "apellidos")
    private String surname;

    @Column(name = "ruc")
    private String ruc;

    @Column(name = "dni")
    private String dni;

    @Column(name = "negocio")
    private String business;

    @Column(name = "celular")
    private String mobile;

    @Column(name = "direccion")
    private String address;

    @Column(name = "correo")
    private String email;

    @Column(name = "estado")
    private Long status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "id_distrito", unique = true)
    private Long id_district;

    @ManyToOne
    @JoinColumn(name = "id_distrito", columnDefinition = "id_district",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_distrito"))
    private District district;

}
