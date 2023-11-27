package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.GenericGenerator;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableDistrict, schema = Constants.schemaMaster)
public class District {
        @Id
        @GeneratedValue(generator = "sequence-district")
        @GenericGenerator(name = "sequence-district", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator", parameters = {
                        @org.hibernate.annotations.Parameter(name = "sequence_name", value = "distritos_sequence"),
                        @org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
                        @org.hibernate.annotations.Parameter(name = "increment_size", value = "1")
        })
        @Column(name = "id_distrito")
        private Long id;

        @Column(name = "nombre", length = 50)
        private String name;

        @Column(name = "estado")
        private boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "id_provincia")
        private Long idProvince;

        @ManyToOne
        @JoinColumn(name = "id_provincia", columnDefinition = "idProvince", insertable = false, updatable = false)
        private Province province;

        @Column(name = "usuario")
        private String user;

}
