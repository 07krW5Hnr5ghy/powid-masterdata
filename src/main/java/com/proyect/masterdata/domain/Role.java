package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;
import java.util.Set;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableRole, schema = Constants.schemaManagement)
public class Role {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_rol", unique = true)
        private Long id;

        @Column(name = "nombre", length = 50, unique = true, nullable = false)
        private String name;

        @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro", nullable = false)
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "fecha_modificacion", nullable = false)
        @CreationTimestamp
        private Date dateUpdate;

        @Column(name = "usuario_token", nullable = false)
        private String tokenUser;
}
