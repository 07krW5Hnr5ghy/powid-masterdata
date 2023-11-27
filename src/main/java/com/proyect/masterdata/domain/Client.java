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
@Table(name = Constants.tableClient, schema = Constants.schemaManagement)
public class Client {

        @Id
        @GeneratedValue(generator = "sequence-client")
        @GenericGenerator(name = "sequence-client", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator", parameters = {
                        @Parameter(name = "sequence_name", value = "cliente_sequence"),
                        @Parameter(name = "initial_value", value = "1"),
                        @Parameter(name = "increment_size", value = "1")
        })
        @Column(name = "id_cliente")
        private Long id;

        @Column(name = "nombre", nullable = false)
        private String name;

        @Column(name = "apellidos", nullable = false)
        private String surname;

        @Column(name = "ruc", nullable = false)
        private String ruc;

        @Column(name = "dni", nullable = false)
        private String dni;

        @Column(name = "negocio", nullable = false)
        private String business;

        @Column(name = "celular", nullable = false)
        private String mobile;

        @Column(name = "direccion", nullable = false)
        private String address;

        @Column(name = "correo", nullable = false)
        private String email;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "id_distrito", nullable = false)
        private Long idDistrict;

        @Column(name = "id_usuario", nullable = false)
        private Long idUser;

        @Column(name = "fecha_registro", nullable = false)
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date dateUpdate;

        @ManyToOne
        @JoinColumn(name = "id_distrito", columnDefinition = "idDistrict", insertable = false, updatable = false)
        private District district;

        @OneToOne
        @JoinColumn(name = "id_usuario", columnDefinition = "idUser", insertable = false, updatable = false)
        private User user;

}
