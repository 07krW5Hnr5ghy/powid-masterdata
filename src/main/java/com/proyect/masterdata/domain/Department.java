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
@Table(name = Constants.tableDepartment, schema = Constants.schemaMaster)
public class Department {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_departamento")
        private Long id;

        @Column(name = "nombre", length = 50)
        private String name;

        @Column(name = "estado")
        private Boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "usuario_token")
        private String tokenUser;

}
