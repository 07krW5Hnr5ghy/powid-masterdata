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
@Table(name = Constants.tableProvince, schema = Constants.schemaMaster)
public class Province {
    @Id
    @GeneratedValue(generator = "sequence-province")
    @GenericGenerator(
            name = "sequence-province",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @org.hibernate.annotations.Parameter(name = "sequence_name", value = "provincias_sequence"),
                    @org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
                    @org.hibernate.annotations.Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_provincia", unique = true)
    private Long id;

    @Column(name="nombre", length=50, unique=true)
    private String name;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "id_departameto")
    private Long idDepartment;

    @Column(name = "usuario")
    private String user;

    @ManyToOne
    @JoinColumn(name = "id_departameto", columnDefinition = "idDepartment",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_departemaneto"))
    private Department department;



}
