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
@Table(name = Constants.tableProvince, schema = Constants.schemaMaster)
public class Province {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_provincia")
        private Long id;

        @Column(name = "nombre", length = 50, nullable = false)
        private String name;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro", nullable = false)
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "id_departameto", nullable = false)
        private Long idDepartment;

        @Column(name = "usuario", nullable = false)
        private String user;

        @ManyToOne
        @JoinColumn(name = "id_departameto", columnDefinition = "idDepartment", insertable = false, updatable = false)
        private Department department;

}
