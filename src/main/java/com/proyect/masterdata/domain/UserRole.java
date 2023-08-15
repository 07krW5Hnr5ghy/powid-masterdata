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
@Table(name= Constants.tableUserRole,schema=Constants.schemaMaster)
public class UserRole {
    @Id
    @GeneratedValue(generator = "sequence-user-role")
    @GenericGenerator(
            name = "sequence-user-role",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "roles_usuario_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )

    @Column(name = "id_rol_usuario", unique = true)
    private Long id;

    @Column(name = "nombre",length=50,unique = true)
    private String name;

    @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean status = true;
    // fecha
    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name="usuario")
    private String user;
}
