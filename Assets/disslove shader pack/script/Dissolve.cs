using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace EasyGameStudio.Jeremy
{
    public class Dissolve : MonoBehaviour
    {

        [Header("show speed")]
        [Range(0, 1f)]
        public float speed_show;

        [Header("hide speed")]
        [Range(0, 1f)]
        public float hide_show;

        private Material _material;
        private bool is_showing;
        private bool is_hiding;
        private float threshold = 0;

        // Start is called before the first frame update
        void Start()
        {
            if (this.GetComponent<MeshRenderer>() == null)
            {
                this._material = this.GetComponent<SkinnedMeshRenderer>().material;
            }
            else
            {
                this._material = this.GetComponent<MeshRenderer>().material;
            }
        }

        // Update is called once per frame
        void Update()
        {
            if (this.is_showing)
            {
                this.threshold = Mathf.Lerp(this.threshold, 0, Time.deltaTime * this.speed_show); 

                if (this.threshold <= 0.1)
                {
                    this.threshold = 0;

                    this.is_showing = false;
                }

                this._material.SetFloat("_threshold", this.threshold);
            }

            if (this.is_hiding)
            {
                this.threshold = Mathf.Lerp(this.threshold, 1, Time.deltaTime * this.speed_show);

                if (this.threshold >= (1 - 0.1))
                {
                    this.threshold = 1;

                    this.is_hiding = false;
                }
                this._material.SetFloat("_threshold", this.threshold);
            }
        }

        public void show()
        {
            this.is_hiding = false;

            this.threshold = 0.9f;

            this._material.SetFloat("_threshold", this.threshold);

            this.is_showing = true;
        }

        public void hide()
        {
            this.is_showing = false;

            this.threshold = 0.1f;

            this._material.SetFloat("_threshold", this.threshold);

            this.is_hiding = true;
        }
    }
}